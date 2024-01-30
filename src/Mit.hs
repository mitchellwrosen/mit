module Mit
  ( main,
  )
where

import Control.Applicative (many)
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable (toList)
import Data.Foldable1 (foldMap1')
import Data.List qualified as List
import Data.List.NonEmpty qualified as List1
import Data.Semigroup qualified as Semigroup
import Data.Text qualified as Text
import Data.Text.Builder.Linear qualified as Text.Builder
import Mit.Command.Branch (mitBranch)
import Mit.Command.Commit (abortIfCouldFastForwardToUpstream, mitCommit)
import Mit.Command.Status (mitStatus)
import Mit.Command.Undo (mitUndo)
import Mit.Git
  ( GitCommitInfo (..),
    GitConflict (..),
    GitConflictXY (..),
    GitVersion (GitVersion),
    git,
    gitApplyStash,
    gitCurrentBranch,
    gitFetch,
    gitMaybeHead,
    gitMergeInProgress,
    gitRemoteBranchHead,
    gitRevParseAbsoluteGitDir,
    gitVersion,
  )
import Mit.Label (Label, goto, label)
import Mit.Logger (Logger, log, makeLogger)
import Mit.Merge (MergeResult (..), mergeResultConflicts, performMerge)
import Mit.Output (Output)
import Mit.Output qualified as Output
import Mit.Prelude
import Mit.Pretty (Pretty)
import Mit.Pretty qualified as Pretty
import Mit.ProcessInfo (ProcessInfo (..))
import Mit.Push
  ( DidntPushReason (NothingToPush, PushWouldBeRejected, PushWouldntReachRemote, TriedToPush),
    PushResult (DidntPush, Pushed),
    performPush,
    pushResultPushed,
  )
import Mit.Seq1 qualified as Seq1
import Mit.Snapshot (performSnapshot, snapshotStash, undoToSnapshot)
import Mit.State (MitState (..), writeMitState)
import Mit.Undo (Undo (..))
import Mit.Verbosity (Verbosity (..), intToVerbosity)
import Options.Applicative qualified as Opt
import Options.Applicative.Types qualified as Opt (Backtracking (Backtrack))
import System.Exit (ExitCode (..), exitWith)
import Text.Builder.ANSI qualified as Text
import Text.Builder.ANSI qualified as Text.Builder
import Text.Printf (printf)

-- FIXME: nicer "git status" story. in particular the conflict markers in the commits after a merge are a bit
-- ephemeral feeling
-- FIXME bail if active cherry-pick, active revert, active rebase, what else?
-- FIXME more Seq, less []

-- TODO mit init
-- TODO mit delete-branch
-- TODO tweak things to work with git < 2.30.1
-- TODO git(hub,lab) flow or something?
-- TODO 'mit branch' with dirty working directory - apply changes to new worktree?
-- TODO undo in more cases?
-- TODO recommend merging master if it conflicts
-- TODO mit log
-- TODO undo revert
-- TODO more specific "undo this change" wording
-- TODO `mit ignore <file>` stops tracking accidentally-added file and adds its name to .git/info/exclude

-- TODO finish porting over to new "output as we go" style
-- TODO move commands into their own modules
-- TODO get rid of "snapshot" concept
-- TODO improve code paths with no remote configured (git remote show-url origin)

main :: IO ()
main = do
  (verbosity, command) <-
    Opt.customExecParser
      Opt.ParserPrefs
        { prefBacktrack = Opt.Backtrack,
          prefColumns = 80,
          prefDisambiguate = True,
          prefHelpLongEquals = False,
          prefHelpShowGlobal = True,
          prefMultiSuffix = "+",
          prefShowHelpOnEmpty = True,
          prefShowHelpOnError = True,
          prefTabulateFill = 24 -- grabbed this from optparse-applicative
        }
      ( Opt.info (Opt.helper <*> parser) $
          Opt.progDesc "mit: a git wrapper with a streamlined UX"
      )

  let output :: Logger Output
      output =
        makeLogger (putPretty . renderOutput)

  let pinfo :: Logger ProcessInfo
      pinfo =
        case verbosity of
          V0 -> makeLogger \_ -> pure ()
          V1 -> makeLogger \info -> Pretty.put (v1 info)
          V2 -> makeLogger \info -> Pretty.put (v1 info <> v2 info)
        where
          v1 :: ProcessInfo -> Pretty
          v1 info =
            Pretty.line $
              Pretty.style (Text.Builder.bold . Text.Builder.brightBlack) $
                let prefix =
                      marker info
                        <> " ["
                        <> Pretty.builder (foldMap Text.Builder.fromChar (printf "%.0f" (info.seconds * (1000 :: Double)) :: [Char]))
                        <> "ms] "
                        <> Pretty.text info.name
                        <> " "
                 in case List1.nonEmpty info.args of
                      Nothing -> prefix
                      Just args1 -> prefix <> sconcat (List1.intersperse (Pretty.char ' ') (quote <$> args1))
          v2 :: ProcessInfo -> Pretty
          v2 info =
            (info.output <> info.errput)
              & Foldable.toList
              & map (Pretty.style Text.Builder.brightBlack . Pretty.text)
              & Pretty.lines
              & Pretty.indent 4

          quote :: Text -> Pretty.Line
          quote s =
            if Text.any Char.isSpace s
              then Pretty.char '\'' <> Pretty.text (Text.replace "'" "\\'" s) <> Pretty.char '\''
              else Pretty.text s

          marker :: ProcessInfo -> Pretty.Line
          marker info =
            case info.exitCode of
              ExitFailure _ -> Pretty.char '✗'
              ExitSuccess -> Pretty.char '✓'

  exitCode <- main1 output pinfo command
  exitWith exitCode
  where
    parser :: Opt.Parser (Verbosity, MitCommand)
    parser =
      (\verbosity command -> (verbosity, command))
        <$> (intToVerbosity . length <$> many (Opt.flag' () (Opt.help "Verbose (-v or -vv)" <> Opt.short 'v')))
        <*> (Opt.hsubparser . fold)
          [ Opt.command "branch" $
              Opt.info
                (MitCommand'Branch <$> Opt.strArgument (Opt.metavar "≪branch≫"))
                (Opt.progDesc "Create a new branch in a new worktree."),
            Opt.command "commit" $
              Opt.info
                ( MitCommand'Commit
                    <$> Opt.switch (Opt.help "Include all changes" <> Opt.long "all")
                    <*> Opt.switch (Opt.help "Don't sync after committing" <> Opt.long "no-sync")
                    <*> Opt.optional
                      ( Opt.strOption $
                          Opt.help "Commit message"
                            <> Opt.long "message"
                            <> Opt.metavar "≪message≫"
                      )
                )
                (Opt.progDesc "Create a commit."),
            -- Opt.command "gc" $
            --   Opt.info
            --     (pure MitCommand'Gc)
            --     (Opt.progDesc "Delete stale, merged branches."),
            Opt.command "merge" $
              Opt.info
                (MitCommand'Merge <$> Opt.strArgument (Opt.metavar "≪branch≫"))
                (Opt.progDesc "Merge the given branch into the current branch."),
            Opt.command "status" $
              Opt.info
                (pure MitCommand'Status)
                (Opt.progDesc "Print file status."),
            Opt.command "sync" $
              Opt.info
                (pure MitCommand'Sync)
                (Opt.progDesc "Sync with the remote named `origin`."),
            Opt.command "undo" $
              Opt.info
                (pure MitCommand'Undo)
                (Opt.progDesc "Undo the last `mit` command (if possible).")
          ]

main1 :: Logger Output -> Logger ProcessInfo -> MitCommand -> IO ExitCode
main1 output pinfo command =
  label \exit -> do
    main2 exit output pinfo command
    pure ExitSuccess

main2 :: Label ExitCode -> Logger Output -> Logger ProcessInfo -> MitCommand -> IO ()
main2 exit output pinfo command = do
  version <- gitVersion pinfo
  -- 'git stash create' broken before 2.30.1
  when (version < GitVersion 2 30 1) do
    log output Output.GitTooOld
    goto exit (ExitFailure 1)

  gitdir <-
    gitRevParseAbsoluteGitDir pinfo & onNothingM do
      log output Output.NoGitDir
      goto exit (ExitFailure 1)

  let sync = mitSync exit output pinfo gitdir
      syncWith = mitSyncWith exit output pinfo gitdir
  case command of
    MitCommand'Branch branch -> mitBranch exit output pinfo branch
    MitCommand'Commit allFlag dontSyncFlag maybeMessage ->
      mitCommit exit output pinfo syncWith gitdir allFlag dontSyncFlag maybeMessage
    MitCommand'Gc -> mitGc
    MitCommand'Merge branch -> mitMerge exit output pinfo gitdir branch
    MitCommand'Status -> mitStatus pinfo
    MitCommand'Sync -> sync
    MitCommand'Undo -> mitUndo exit output pinfo sync gitdir

data MitCommand
  = MitCommand'Branch !Text
  | MitCommand'Commit !Bool !Bool !(Maybe Text)
  | MitCommand'Gc
  | MitCommand'Merge !Text
  | MitCommand'Status
  | MitCommand'Sync
  | MitCommand'Undo

mitGc :: IO ()
mitGc =
  pure ()

mitMerge :: Label ExitCode -> Logger Output -> Logger ProcessInfo -> Text -> Text -> IO ()
mitMerge exit output pinfo gitdir source = do
  whenM (gitMergeInProgress gitdir) do
    log output Output.MergeInProgress
    goto exit (ExitFailure 1)

  branch <-
    gitCurrentBranch pinfo & onNothingM do
      log output Output.NotOnBranch
      goto exit (ExitFailure 1)

  let upstream = "origin/" <> branch

  -- Special case: if on branch "foo", treat `mit merge foo` and `mit merge origin/foo` as `mit sync`
  case source == branch || source == upstream of
    True -> mitSyncWith exit output pinfo gitdir Nothing
    False -> mitMerge_ exit output pinfo gitdir source branch

mitMerge_ :: Label ExitCode -> Logger Output -> Logger ProcessInfo -> Text -> Text -> Text -> IO ()
mitMerge_ exit output pinfo gitdir source branch = do
  fetched <- gitFetch pinfo "origin"
  maybeHead0 <- gitMaybeHead pinfo
  maybeUpstreamHead <- gitRemoteBranchHead pinfo "origin" branch
  snapshot <- performSnapshot pinfo

  -- When given 'mit merge foo', prefer running 'git merge origin/foo' over 'git merge foo'
  sourceCommit <-
    gitRemoteBranchHead pinfo "origin" source & onNothingM do
      git pinfo ["rev-parse", "refs/heads/" <> source] & onLeftM \_ -> do
        log output Output.NoSuchBranch
        goto exit (ExitFailure 1)

  abortIfCouldFastForwardToUpstream exit output pinfo maybeHead0 maybeUpstreamHead fetched

  whenJust (snapshotStash snapshot) \_stash ->
    git @() pinfo ["reset", "--hard", "--quiet", "HEAD"]

  mergeResult <- performMerge pinfo gitdir sourceCommit ("⅄ " <> source <> " → " <> branch)

  log output case mergeResult of
    NothingToMerge -> Output.NothingToMerge source branch
    TriedToMerge commits conflicts -> Output.MergeFailed commits conflicts
    Merged commits -> Output.MergeSucceeded (Just commits)

  head1 <- git pinfo ["rev-parse", "HEAD"] -- FIXME oops this can be Nothing
  pushResult <- performPush pinfo branch (Just head1) maybeUpstreamHead fetched

  case pushResult of
    DidntPush NothingToPush -> pure ()
    DidntPush (PushWouldBeRejected localCommits numRemoteCommits) ->
      log output (Output.PushWouldBeRejected localCommits numRemoteCommits)
    DidntPush (PushWouldntReachRemote commits) -> log output (Output.PushWouldntReachRemote commits)
    DidntPush (TriedToPush commits) -> log output (Output.PushFailed commits)
    Pushed commits -> log output (Output.PushSucceeded commits)

  let undo1 =
        if pushResultPushed pushResult || isNothing (mergeResultConflicts mergeResult)
          then Nothing
          else undoToSnapshot snapshot

  writeMitState
    gitdir
    branch
    MitState
      { head = head1,
        merging =
          case mergeResultConflicts mergeResult of
            Nothing -> Nothing
            Just _conflicts -> Just source,
        undo = undo1
      }

  when (isNothing (mergeResultConflicts mergeResult)) do
    whenJust (snapshotStash snapshot) \stash -> do
      conflicts0 <- gitApplyStash pinfo stash
      whenJust (Seq1.fromList conflicts0) \conflicts1 -> log output (Output.UnstashFailed conflicts1)

  when (isJust undo1) (log output Output.CanUndo)

-- TODO implement "lateral sync", i.e. a merge from some local or remote branch, followed by a sync to upstream
mitSync :: Label ExitCode -> Logger Output -> Logger ProcessInfo -> Text -> IO ()
mitSync exit output pinfo gitdir = do
  whenM (gitMergeInProgress gitdir) do
    log output Output.MergeInProgress
    goto exit (ExitFailure 1)
  mitSyncWith exit output pinfo gitdir Nothing

-- | @mitSyncWith _ maybeUndos@
--
-- Whenever recording what 'mit undo' should do after 'mit sync', if 'maybeUndos' is provided, we use them instead.
-- This is pulled into a function argument to get better undo behavior after committing a merge.
--
-- Consider:
--
-- The user runs 'mit merge foo' (with or without a clean working tree), and gets conflicts. After fixing them, she runs
-- 'mit commit'. This may result in *additional* conflicts due to the just-stashed uncommitted changes.
--
-- But either way, internally, we would like this 'mit commit' to effectively behave as a normal commit, in the sense
-- that we want to immediately push it upstream. That means the code would like to simply call 'mit sync' after
-- 'git commit'!
--
-- However, if this commit could be undone (i.e. we didn't push it), we wouldn't want that 'mit sync' to *locally*
-- compute where to undo, because it would just conclude, "oh, HEAD hasn't moved, and we didn't push, so there's nothing
-- to undo".
--
-- Instead, we want to undo to the point before running the 'mit merge' that caused the conflicts, which were later
-- resolved by 'mit commit'.
mitSyncWith :: Label ExitCode -> Logger Output -> Logger ProcessInfo -> Text -> Maybe Undo -> IO ()
mitSyncWith exit output pinfo gitdir maybeUndo = do
  fetched <- gitFetch pinfo "origin"
  branch <- do
    gitCurrentBranch pinfo & onNothingM do
      log output Output.NotOnBranch
      goto exit (ExitFailure 1)
  maybeUpstreamHead <- gitRemoteBranchHead pinfo "origin" branch
  snapshot <- performSnapshot pinfo

  whenJust (snapshotStash snapshot) \_stash ->
    git @() pinfo ["reset", "--hard", "--quiet", "HEAD"]

  mergeResult <-
    case maybeUpstreamHead of
      -- Yay: no upstream branch is not different from an up-to-date local branch
      Nothing -> pure NothingToMerge
      Just upstreamHead -> performMerge pinfo gitdir upstreamHead ("⅄ " <> branch)

  case mergeResult of
    NothingToMerge -> pure ()
    TriedToMerge commits conflicts -> log output (Output.PullFailed commits conflicts)
    Merged commits -> log output (Output.PullSucceeded commits)

  maybeHead1 <- gitMaybeHead pinfo

  pushResult <- performPush pinfo branch maybeHead1 maybeUpstreamHead fetched

  case pushResult of
    DidntPush NothingToPush -> pure ()
    DidntPush (PushWouldBeRejected localCommits numRemoteCommits) ->
      log output (Output.PushWouldBeRejected localCommits numRemoteCommits)
    DidntPush (PushWouldntReachRemote commits) -> log output (Output.PushWouldntReachRemote commits)
    DidntPush (TriedToPush commits) -> log output (Output.PushFailed commits)
    Pushed commits -> log output (Output.PushSucceeded commits)

  let undo1 =
        case (pushResultPushed pushResult, maybeUndo, mergeResultConflicts mergeResult) of
          (True, _, _) -> Nothing
          -- FIXME hm, could consider appending those undos instead, even if they obviate the recent stash/merge
          -- undos
          (False, Just undo, _) -> Just undo
          (False, Nothing, Nothing) -> Nothing
          (False, Nothing, Just _conflicts) -> undoToSnapshot snapshot

  whenJust maybeHead1 \head1 ->
    writeMitState
      gitdir
      branch
      MitState
        { head = head1,
          merging =
            case mergeResultConflicts mergeResult of
              Nothing -> Nothing
              Just _conflicts -> Just branch,
          undo = undo1
        }

  when (isNothing (mergeResultConflicts mergeResult)) do
    whenJust (snapshotStash snapshot) \stash -> do
      conflicts0 <- gitApplyStash pinfo stash
      whenJust (Seq1.fromList conflicts0) \conflicts1 -> log output (Output.UnstashFailed conflicts1)

  when (isJust undo1) (log output Output.CanUndo)

putPretty :: Pretty -> IO ()
putPretty p =
  Pretty.put (emptyLine <> Pretty.indent 2 p <> emptyLine)
  where
    emptyLine = Pretty.line (Pretty.char ' ')

------------------------------------------------------------------------------------------------------------------------
-- Rendering output to the terminal

renderOutput :: Output -> Pretty
renderOutput = \case
  Output.BranchAlreadyCheckedOut branch dir1 dir2 ->
    Pretty.line $
      Pretty.style Text.red $
        "✗ "
          <> Pretty.branch branch
          <> " is already checked out in "
          <> Pretty.directory dir2
          <> ", so I can't check it out again in "
          <> Pretty.directory dir1
          <> "."
  Output.CanUndo -> Pretty.line ("Run " <> Pretty.command "mit undo" <> " to undo this change.")
  Output.CheckedOutBranch branch directory ->
    Pretty.line ("✓ I checked out " <> Pretty.branch branch <> " in " <> Pretty.directory directory)
  Output.CreatedBranch branch directory ->
    Pretty.line ("✓ I created " <> Pretty.branch branch <> " in " <> Pretty.directory directory <> ".")
  Output.DirectoryAlreadyExists branch directory ->
    Pretty.line $
      Pretty.style Text.red $
        "✗ I can't check out "
          <> Pretty.branch branch
          <> " in "
          <> Pretty.directory directory
          <> ", because the directory already exists."
  Output.GitTooOld -> Pretty.line (Pretty.style Text.red "✗ I require git version 2.30.1 or later.")
  Output.MergeFailed commits conflicts ->
    Pretty.lines $
      Pretty.style
        Text.red
        ( "✗ I tried to merge "
            <> commitsN (Seq1.length commits)
            <> ", but there are conflicts."
        )
        : map (\conflict -> "  " <> Pretty.style Text.red (prettyGitConflict conflict)) (Seq1.toList conflicts)
  Output.MergeSucceeded maybeCommits ->
    case maybeCommits of
      Nothing -> Pretty.line (Pretty.style Text.green "✓ I merged some commits.")
      Just commits ->
        Pretty.lines $
          Pretty.style Text.green ("✓ I merged " <> commitsN (Seq1.length commits) <> ".")
            : Pretty.indent 2 (prettyCommits commits)
  Output.MergeInProgress ->
    Pretty.line $
      Pretty.style Text.red $
        "✗ There's currently a merge in progress that has to be resolved first."
  Output.NoGitDir -> Pretty.line (Pretty.style Text.red "✗ The current directory doesn't contain a git repository.")
  Output.NoSuchBranch -> Pretty.line (Pretty.style Text.red "✗ No such branch.")
  Output.NotOnBranch -> Pretty.line (Pretty.style Text.red "✗ You are not on a branch.")
  Output.NothingToCommit -> Pretty.line (Pretty.style Text.red "✗ There's nothing to commit.")
  Output.NothingToMerge source target ->
    Pretty.line $
      Pretty.style Text.green $
        "✓ " <> Pretty.branch target <> " is already up-to-date with " <> Pretty.branch source <> "."
  Output.NothingToUndo -> Pretty.line (Pretty.style Text.red "✗ There's nothing to undo.")
  Output.PullFailed commits conflicts ->
    Pretty.lines $
      Pretty.style
        Text.red
        ( "✗ I tried to pull "
            <> commitsN (Seq1.length commits)
            <> ", but there are conflicts."
        )
        : map (\conflict -> "  " <> Pretty.style Text.red (prettyGitConflict conflict)) (Seq1.toList conflicts)
  Output.PullSucceeded commits ->
    Pretty.lines $
      Pretty.style Text.green ("✓ I pulled " <> commitsN (Seq1.length commits) <> ".")
        : Pretty.indent 2 (prettyCommits commits)
  Output.PushFailed commits ->
    Pretty.line (Pretty.style Text.red ("✗ I tried to push " <> commitsN (Seq1.length commits) <> ", but failed."))
  Output.PushSucceeded commits ->
    Pretty.lines $
      Pretty.style Text.green ("✓ I pushed " <> commitsN (Seq1.length commits) <> ".")
        : Pretty.indent 2 (prettyCommits commits)
  Output.PushWouldBeRejected localCommits numRemoteCommits ->
    Pretty.line
      ( Pretty.style
          Text.red
          ( "✗ I didn't try to push "
              <> commitsN (Seq1.length localCommits)
              <> ", because there "
              <> commitsVN numRemoteCommits
              <> " to pull first."
          )
      )
  Output.PushWouldntReachRemote commits ->
    Pretty.line
      ( Pretty.style
          Text.red
          ("✗ I didn't try to push " <> commitsN (Seq1.length commits) <> ", because you appear to be offline.")
      )
  Output.UnstashFailed conflicts ->
    Pretty.lines $
      Pretty.style Text.red ("✗ I tried to restore your uncommitted changes, but there are conflicts.")
        : map (\conflict -> "  " <> Pretty.style Text.red (prettyGitConflict conflict)) (Seq1.toList conflicts)
  Output.UpstreamIsAhead numRemoteCommits ->
    Pretty.line (Pretty.style Text.red ("✗ There " <> commitsVN numRemoteCommits <> " to pull first."))
  where
    commitsN :: Int -> Pretty.Line
    commitsN = \case
      1 -> "1 commit"
      n -> Pretty.int n <> " commits"

    commitsVN :: Int -> Pretty.Line
    commitsVN = \case
      1 -> "is 1 commit"
      n -> "are " <> Pretty.int n <> " commits"

prettyCommits :: Seq1 GitCommitInfo -> Pretty
prettyCommits commits =
  Pretty.lines $
    if Seq1.length commits <= 10
      then f (List.take 10 (Seq1.toList commits))
      else f (List.take 8 (Seq1.toList commits)) ++ ["│ ...", p (Seq1.last commits)]
  where
    f :: [GitCommitInfo] -> [Pretty.Line]
    f =
      snd . List.mapAccumL g ""

    g :: Text -> GitCommitInfo -> (Text, Pretty.Line)
    g previousDate commit
      | commit.date == previousDate = (previousDate, p commit {date = ""})
      | otherwise = (commit.date, p commit)

    p :: GitCommitInfo -> Pretty.Line
    p commit =
      "│ " <> prettyGitCommitInfo dateWidth commit

    dateWidth :: Int
    dateWidth =
      commits
        & foldMap1' (\commit -> Semigroup.Max (Text.length commit.date))
        & Semigroup.getMax

prettyGitCommitInfo :: Int -> GitCommitInfo -> Pretty.Line
prettyGitCommitInfo dateWidth info =
  Pretty.style (Text.Builder.bold . Text.Builder.black) (Pretty.text info.shorthash)
    <> Semigroup.stimes (dateWidth - thisDateWidth + 1) (Pretty.char ' ')
    <> Pretty.style (Text.Builder.italic . Text.Builder.yellow) (Pretty.text info.date)
    <> Pretty.char ' '
    <> Pretty.style (Text.Builder.bold . Text.Builder.white) (Pretty.text info.subject)
    <> " - "
    <> Pretty.style (Text.Builder.italic . Text.Builder.white) (Pretty.text info.author)
  where
    thisDateWidth = Text.length info.date

prettyGitConflict :: GitConflict -> Pretty.Line
prettyGitConflict (GitConflict xy name) =
  Pretty.text name <> " (" <> prettyGitConflictXY xy <> ")"

prettyGitConflictXY :: GitConflictXY -> Pretty.Line
prettyGitConflictXY = \case
  AA -> "both added"
  AU -> "added by us"
  DD -> "both deleted"
  DU -> "deleted by us"
  UA -> "added by them"
  UD -> "deleted by them"
  UU -> "both modified"
