module Mit
  ( main,
  )
where

import Control.Applicative (many)
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable (toList)
import Data.List qualified as List
import Data.List.NonEmpty qualified as List1
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Builder.Linear qualified as Text (Builder)
import Data.Text.Builder.Linear qualified as Text.Builder
import Mit.Command.Branch (mitBranch)
import Mit.Command.Status (mitStatus)
import Mit.Command.Undo (mitUndo)
import Mit.Git
  ( DiffResult (Differences, NoDifferences),
    GitCommitInfo (..),
    GitConflict (..),
    GitConflictXY (..),
    GitVersion (GitVersion),
    git,
    git2,
    gitApplyStash,
    gitCommitsBetween,
    gitConflictsWith,
    gitCurrentBranch,
    gitDiff,
    gitExistCommitsBetween,
    gitFetch,
    gitIsMergeCommit,
    gitMaybeHead,
    gitMergeInProgress,
    gitRemoteBranchHead,
    gitRevParseAbsoluteGitDir,
    gitUnstageChanges,
    gitVersion,
  )
import Mit.Label (Label, goto, label)
import Mit.Logger (Logger, log, makeLogger)
import Mit.Merge (MergeResult (..), mergeResultCommits, mergeResultConflicts, performMerge)
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
    pushResultCommits,
    pushResultPushed,
  )
import Mit.Seq1 qualified as Seq1
import Mit.Snapshot (Snapshot, performSnapshot, snapshotStash, undoToSnapshot)
import Mit.State (MitState (..), readMitState, writeMitState)
import Mit.Undo (Undo (..), concatUndos, undoStash)
import Mit.Verbosity (Verbosity (..), intToVerbosity)
import Options.Applicative qualified as Opt
import Options.Applicative.Types qualified as Opt (Backtracking (Backtrack))
import System.Exit (ExitCode (..), exitWith)
import System.Posix.Terminal (queryTerminal)
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
  whenNotM (gitRevParseAbsoluteGitDir pinfo) do
    log output Output.NoGitDir
    goto exit (ExitFailure 1)

  let sync = mitSync exit output pinfo
  case command of
    MitCommand'Branch branch -> mitBranch exit output pinfo branch
    MitCommand'Commit allFlag maybeMessage -> mitCommit exit output pinfo allFlag maybeMessage
    MitCommand'Gc -> mitGc
    MitCommand'Merge branch -> mitMerge exit output pinfo branch
    MitCommand'Status -> mitStatus pinfo
    MitCommand'Sync -> sync
    MitCommand'Undo -> mitUndo exit output pinfo sync

data MitCommand
  = MitCommand'Branch !Text
  | MitCommand'Commit
      !Bool -- all?
      !(Maybe Text) -- message
  | MitCommand'Gc
  | MitCommand'Merge !Text
  | MitCommand'Status
  | MitCommand'Sync
  | MitCommand'Undo

mitCommit :: Label ExitCode -> Logger Output -> Logger ProcessInfo -> Bool -> Maybe Text -> IO ()
mitCommit exit output pinfo allFlag maybeMessage = do
  gitMergeInProgress pinfo >>= \case
    False -> mitCommitNotMerge exit output pinfo allFlag maybeMessage
    True -> mitCommitMerge exit output pinfo

mitCommitNotMerge :: Label ExitCode -> Logger Output -> Logger ProcessInfo -> Bool -> Maybe Text -> IO ()
mitCommitNotMerge exit output pinfo allFlag maybeMessage = do
  -- Check to see if there's even anything to commit, and bail if not.
  gitUnstageChanges pinfo
  gitDiff pinfo >>= \case
    Differences -> pure ()
    NoDifferences -> do
      log output Output.NothingToCommit
      goto exit (ExitFailure 1)

  fetched <- gitFetch pinfo "origin"
  branch <-
    gitCurrentBranch pinfo & onNothingM do
      log output Output.NotOnBranch
      goto exit (ExitFailure 1)
  maybeHead0 <- gitMaybeHead pinfo
  let upstream = "origin/" <> branch
  maybeUpstreamHead <- gitRemoteBranchHead pinfo "origin" branch
  state0 <- readMitState pinfo branch
  snapshot <- performSnapshot pinfo

  abortIfCouldFastForwardToUpstream exit output pinfo branch maybeHead0 upstream maybeUpstreamHead fetched

  -- Initiate a commit, which (if interactive) can be cancelled with Ctrl+C.
  committed <- do
    doCommitAll <- if allFlag then pure True else not <$> queryTerminal 0
    case (doCommitAll, maybeMessage) of
      (True, Nothing) -> git2 pinfo ["commit", "--all", "--quiet"]
      (True, Just message) -> git pinfo ["commit", "--all", "--message", message, "--quiet"]
      (False, Nothing) -> git2 pinfo ["commit", "--patch", "--quiet"]
      (False, Just message) -> git2 pinfo ["commit", "--patch", "--message", message, "--quiet"]

  -- Get the new head after the commit (if successful)
  maybeHead1 <- if committed then Just <$> git pinfo ["rev-parse", "HEAD"] else pure maybeHead0

  -- Attempt a push (even if the commit was cancelled, since we might have other unpublished commits).
  pushResult <- performPush pinfo branch maybeHead1 maybeUpstreamHead fetched

  case pushResult of
    DidntPush NothingToPush -> pure ()
    DidntPush (PushWouldBeRejected localCommits numRemoteCommits) ->
      log output (Output.PushWouldBeRejected localCommits numRemoteCommits)
    DidntPush (PushWouldntReachRemote commits) -> log output (Output.PushWouldntReachRemote commits)
    DidntPush (TriedToPush commits) -> log output (Output.PushFailed commits)
    Pushed commits -> log output (Output.PushSucceeded commits)

  whenJust maybeHead1 \head1 -> do
    maybeUndoPush <-
      case pushResult of
        Pushed commits ->
          case Seq1.toList commits of
            [commit] ->
              gitIsMergeCommit pinfo commit.hash <&> \case
                False -> Just (Revert commit.hash Nothing)
                True -> Nothing
            _ -> pure Nothing
        DidntPush _reason -> pure Nothing

    let maybeUndo1 =
          case (pushResultPushed pushResult, committed) of
            (False, False) -> state0.undo
            (False, True) -> undoToSnapshot snapshot
            (True, False) -> maybeUndoPush
            (True, True) -> do
              -- If we can revert the push *and* there is a stash in the snapshot (i.e. this *isnt* the very first
              -- commit), then we can undo (by reverting then applying the stash).
              --
              -- But if (for example) we can revert the push but there is *not* a stash in the snapshot, that means
              -- there were no commits before this one (`git stash create` is illegal there), so we don't want to
              -- offer to undo, because although we can revert the commit, we have no way of getting from there to
              -- back to having some dirty stuff to commit.
              undoPush <- maybeUndoPush
              stash <- snapshotStash snapshot
              Just (concatUndos undoPush (Apply stash Nothing))

    writeMitState pinfo branch MitState {head = head1, merging = Nothing, undo = maybeUndo1}

    -- Whether we say we can undo from here is not exactly if the state says we can undo, because of one corner case: we
    -- ran 'mit commit', then aborted the commit, and ultimately didn't push any other local changes.
    --
    -- In this case, the underlying state hasn't changed, so 'mit undo' will still work as if the 'mit commit' was never
    -- run, we merely don't want to *say* "run 'mit undo' to undo" as feedback, because that sounds as if it would undo
    -- the last command run, namely the 'mit commit' that was aborted.
    when (isJust maybeUndo1 && committed) do
      putPretty canUndoStanza

mitCommitMerge :: Label ExitCode -> Logger Output -> Logger ProcessInfo -> IO ()
mitCommitMerge exit output pinfo = do
  branch <-
    gitCurrentBranch pinfo & onNothingM do
      log output Output.NotOnBranch
      goto exit (ExitFailure 1)
  state0 <- readMitState pinfo branch
  head0 <- git pinfo ["rev-parse", "HEAD"]

  -- Make the merge commit. Commonly we'll have gotten here by `mit merge <branch>`, so we'll have a `state0.merging`
  -- that tells us we're merging in <branch>. But we also handle the case that we went `git merge` -> `mit commit`,
  -- because why not.
  case state0.merging of
    Nothing -> git @() pinfo ["commit", "--all", "--no-edit"]
    Just source ->
      git @()
        pinfo
        [ "commit",
          "--all",
          "--message",
          fold ["⅄ ", if source == branch then "" else source <> " → ", branch]
        ]

  head1 <- git pinfo ["rev-parse", "HEAD"]

  -- Record that we are no longer merging. FIXME what about undos?
  let state1 = state0 {head = head1, merging = Nothing}
  writeMitState pinfo branch state1

  whenJust state0.merging \source ->
    when (source /= branch) (log output (Output.MergeSucceeded source branch Nothing))

  -- Three possible cases:
  --   1. We had a clean working directory before `mit merge`, so proceed to sync
  --   2. We had a dirty working directory before `mit merge` (evidence: our undo has a `git stash apply` in it)
  --     a. We can cleanly unstash it, so proceed to sync
  --     b. We cannot cleanly unstash it, so don't sync, because that may *further* conflict, and we don't want nasty
  --        double conflict markers

  case state0.undo >>= undoStash of
    Nothing -> mitSyncWith exit output pinfo (Just (Reset head0 Nothing))
    Just stash -> do
      conflicts <- gitApplyStash pinfo stash
      case List1.nonEmpty conflicts of
        -- FIXME we just unstashed, now we're about to stash again :/
        Nothing -> mitSyncWith exit output pinfo (Just (Reset head0 (Just (Apply stash Nothing))))
        Just conflicts1 ->
          putPretty $
            Pretty.paragraphs
              [ conflictsStanza "These files are in conflict:" conflicts1,
                Pretty.line $
                  "Resolve the conflicts, then run "
                    <> Pretty.command "mit commit"
                    <> " to synchronize "
                    <> Pretty.branch branch
                    <> " with "
                    <> Pretty.branch ("origin/" <> branch)
                    <> ".",
                Pretty.when (isJust state0.undo) canUndoStanza
              ]

mitGc :: IO ()
mitGc =
  pure ()

mitMerge :: Label ExitCode -> Logger Output -> Logger ProcessInfo -> Text -> IO ()
mitMerge exit output pinfo source = do
  whenM (gitMergeInProgress pinfo) do
    log output Output.MergeInProgress
    goto exit (ExitFailure 1)

  branch <-
    gitCurrentBranch pinfo & onNothingM do
      log output Output.NotOnBranch
      goto exit (ExitFailure 1)
  let upstream = "origin/" <> branch

  -- Special case: if on branch "foo", treat `mit merge foo` and `mit merge origin/foo` as `mit sync`
  case source == branch || source == upstream of
    True -> mitSyncWith exit output pinfo Nothing
    False -> mitMerge_ exit output pinfo source branch upstream

mitMerge_ :: Label ExitCode -> Logger Output -> Logger ProcessInfo -> Text -> Text -> Text -> IO ()
mitMerge_ exit output pinfo source branch upstream = do
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

  abortIfCouldFastForwardToUpstream exit output pinfo branch maybeHead0 upstream maybeUpstreamHead fetched

  cleanWorkingTree pinfo snapshot

  mergeResult <- performMerge pinfo sourceCommit ("⅄ " <> source <> " → " <> branch)

  log output case mergeResult of
    NothingToMerge -> Output.NothingToMerge source branch
    TriedToMerge commits _conflicts -> Output.MergeFailed source branch commits
    Merged commits -> Output.MergeSucceeded source branch (Just commits)

  stashConflicts <-
    case (mergeResultConflicts mergeResult, snapshotStash snapshot) of
      (Nothing, Just stash) -> gitApplyStash pinfo stash
      _ -> pure []

  head1 <- git pinfo ["rev-parse", "HEAD"]
  pushResult <- performPush pinfo branch (Just head1) maybeUpstreamHead fetched
  let undo1 =
        if pushResultPushed pushResult || isNothing (mergeResultConflicts mergeResult)
          then Nothing
          else undoToSnapshot snapshot

  writeMitState
    pinfo
    branch
    MitState
      { head = head1,
        merging =
          case mergeResultConflicts mergeResult of
            Nothing -> Nothing
            Just _conflicts -> Just source,
        undo = undo1
      }

  remoteCommits <-
    case maybeUpstreamHead of
      Nothing -> pure Seq.empty
      Just upstreamHead -> gitCommitsBetween pinfo (Just "HEAD") upstreamHead

  conflictsOnSync <-
    if Seq.null remoteCommits
      then pure []
      else gitConflictsWith pinfo (fromJust maybeUpstreamHead)

  putPretty $
    Pretty.paragraphs
      [ isSynchronizedStanza branch pushResult,
        Pretty.whenJust (Seq1.fromSeq remoteCommits) \commits ->
          syncStanza Sync {commits, success = False, source = upstream, target = branch},
        -- TODO show commits to remote
        case (Seq1.toList1 <$> mergeResultConflicts mergeResult) <|> List1.nonEmpty stashConflicts of
          Nothing -> Pretty.empty
          Just conflicts1 -> conflictsStanza "These files are in conflict:" conflicts1,
        -- TODO audit this
        case pushResult of
          DidntPush NothingToPush -> Pretty.empty
          DidntPush (PushWouldntReachRemote _) -> runSyncStanza "When you come online, run" branch upstream
          -- FIXME hrm, but we might have merge conflicts and/or stash conflicts!
          DidntPush (PushWouldBeRejected _ _) ->
            case List1.nonEmpty conflictsOnSync of
              Nothing -> runSyncStanza "Run" branch upstream
              Just conflictsOnSync1 ->
                Pretty.paragraphs
                  [ conflictsStanza
                      ("These files will be in conflict when you run " <> Pretty.command "mit sync" <> ":")
                      conflictsOnSync1,
                    Pretty.line $
                      "Run "
                        <> Pretty.command "mit sync"
                        <> ", resolve the conflicts, then run "
                        <> Pretty.command "mit commit"
                        <> " to synchronize "
                        <> Pretty.branch branch
                        <> " with "
                        <> Pretty.branch upstream
                        <> "."
                  ]
          DidntPush (TriedToPush _) -> runSyncStanza "Run" branch upstream
          Pushed _ -> Pretty.empty,
        Pretty.when (isJust undo1) canUndoStanza
      ]

-- TODO implement "lateral sync", i.e. a merge from some local or remote branch, followed by a sync to upstream
mitSync :: Label ExitCode -> Logger Output -> Logger ProcessInfo -> IO ()
mitSync exit output pinfo = do
  whenM (gitMergeInProgress pinfo) do
    log output Output.MergeInProgress
    goto exit (ExitFailure 1)
  mitSyncWith exit output pinfo Nothing

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
mitSyncWith :: Label ExitCode -> Logger Output -> Logger ProcessInfo -> Maybe Undo -> IO ()
mitSyncWith exit output pinfo maybeUndo = do
  fetched <- gitFetch pinfo "origin"
  branch <- do
    gitCurrentBranch pinfo & onNothingM do
      log output Output.NotOnBranch
      goto exit (ExitFailure 1)
  let upstream = "origin/" <> branch
  maybeUpstreamHead <- gitRemoteBranchHead pinfo "origin" branch
  snapshot <- performSnapshot pinfo

  cleanWorkingTree pinfo snapshot

  mergeResult <-
    case maybeUpstreamHead of
      -- Yay: no upstream branch is not different from an up-to-date local branch
      Nothing -> pure NothingToMerge
      Just upstreamHead -> performMerge pinfo upstreamHead ("⅄ " <> branch)

  stashConflicts <-
    case (mergeResultConflicts mergeResult, snapshotStash snapshot) of
      (Nothing, Just stash) -> gitApplyStash pinfo stash
      _ -> pure []

  head1 <- git pinfo ["rev-parse", "HEAD"] -- can this be Nothing?
  pushResult <- performPush pinfo branch (Just head1) maybeUpstreamHead fetched
  let undo1 =
        case (pushResultPushed pushResult, maybeUndo, mergeResultConflicts mergeResult) of
          (True, _, _) -> Nothing
          (False, Nothing, Nothing) -> Nothing
          (False, Nothing, Just _conflicts) -> undoToSnapshot snapshot
          -- FIXME hm, could consider appending those undos instead, even if they obviate the recent stash/merge
          -- undos
          (False, Just undo, _) -> Just undo

  writeMitState
    pinfo
    branch
    MitState
      { head = head1,
        merging =
          case mergeResultConflicts mergeResult of
            Nothing -> Nothing
            Just _conflicts -> Just branch,
        undo = undo1
      }

  putPretty $
    Pretty.paragraphs
      [ isSynchronizedStanza branch pushResult,
        Pretty.whenJust (mergeResultCommits mergeResult) \commits ->
          syncStanza
            Sync
              { commits,
                success =
                  case mergeResultConflicts mergeResult of
                    Nothing -> True
                    Just _conflicts -> False,
                source = upstream,
                target = branch
              },
        Pretty.whenJust (pushResultCommits pushResult) \commits ->
          syncStanza
            Sync
              { commits,
                success = pushResultPushed pushResult,
                source = branch,
                target = upstream
              },
        Pretty.whenJust ((Seq1.toList1 <$> mergeResultConflicts mergeResult) <|> List1.nonEmpty stashConflicts) \conflicts1 ->
          conflictsStanza "These files are in conflict:" conflicts1,
        case pushResult of
          DidntPush NothingToPush -> Pretty.empty
          DidntPush (PushWouldntReachRemote _) -> runSyncStanza "When you come online, run" branch upstream
          DidntPush (PushWouldBeRejected _ _) ->
            Pretty.line $
              "Resolve the conflicts, then run "
                <> Pretty.command "mit commit"
                <> " to synchronize "
                <> Pretty.branch branch
                <> " with "
                <> Pretty.branch upstream
                <> "."
          DidntPush (TriedToPush _) -> runSyncStanza "Run" branch upstream
          Pushed _ -> Pretty.empty,
        Pretty.when (isJust undo1) canUndoStanza
      ]

-- If origin/branch is strictly ahead of branch (so we could fast-forward), abort, but if we successfully fetched,
-- because we do want to allow offline activity regardless.
abortIfCouldFastForwardToUpstream ::
  Label ExitCode ->
  Logger Output ->
  Logger ProcessInfo ->
  Text ->
  Maybe Text ->
  Text ->
  Maybe Text ->
  Bool ->
  IO ()
abortIfCouldFastForwardToUpstream exit output pinfo branch maybeHead upstream maybeUpstreamHead fetched = do
  when fetched do
    whenJust maybeUpstreamHead \upstreamHead -> do
      head <- maybeHead & onNothing upstreamIsAhead
      whenM (gitExistCommitsBetween pinfo head upstreamHead) do
        whenNotM (gitExistCommitsBetween pinfo upstreamHead head) upstreamIsAhead
  where
    upstreamIsAhead :: IO void
    upstreamIsAhead = do
      log output (Output.UpstreamIsAhead branch upstream)
      goto exit (ExitFailure 1)

-- Clean the working tree, if it's dirty (it's been stashed).
cleanWorkingTree :: Logger ProcessInfo -> Snapshot -> IO ()
cleanWorkingTree pinfo snapshot = do
  whenJust (snapshotStash snapshot) \_stash ->
    git @() pinfo ["reset", "--hard", "--quiet", "HEAD"]

putPretty :: Pretty -> IO ()
putPretty p =
  Pretty.put (emptyLine <> Pretty.indent 2 p <> emptyLine)
  where
    emptyLine = Pretty.line (Pretty.char ' ')

-- FIXME this type kinda sux now, replace with GitMerge probably?
data Sync = Sync
  { commits :: !(Seq1 GitCommitInfo),
    success :: !Bool,
    source :: !Text,
    target :: !Text
  }

canUndoStanza :: Pretty
canUndoStanza =
  Pretty.line ("Run " <> Pretty.command "mit undo" <> " to undo this change.")

conflictsStanza :: Pretty.Line -> List1 GitConflict -> Pretty
conflictsStanza prefix conflicts =
  Pretty.lines $
    prefix
      : map f (List1.toList conflicts)
  where
    f conflict =
      "  " <> Pretty.style Text.red (prettyGitConflict conflict)

isSynchronizedStanza :: Text -> PushResult -> Pretty
isSynchronizedStanza branch = \case
  DidntPush NothingToPush -> synchronized
  DidntPush (PushWouldntReachRemote _) -> notSynchronizedStanza branch upstream " (you appear to be offline)"
  DidntPush (PushWouldBeRejected _ _) -> notSynchronizedStanza branch upstream " (their histories have diverged)"
  DidntPush (TriedToPush _) ->
    notSynchronizedStanza branch upstream (" (" <> Pretty.style Text.bold "git push" <> " failed)")
  Pushed _ -> synchronized
  where
    upstream = "origin/" <> branch

    synchronized =
      ("✓ " <> Pretty.branch branch <> " ≡ " <> Pretty.branch upstream)
        & Pretty.style Text.green
        & Pretty.line

notSynchronizedStanza :: Text -> Text -> Pretty.Line -> Pretty
notSynchronizedStanza branch other suffix =
  ("✗ " <> Pretty.branch branch <> " ≢ " <> Pretty.branch other <> suffix)
    & Pretty.style Text.red
    & Pretty.line

runSyncStanza :: Pretty.Line -> Text -> Text -> Pretty
runSyncStanza prefix branch upstream =
  Pretty.line $
    prefix
      <> " "
      <> Pretty.command "mit sync"
      <> " to synchronize "
      <> Pretty.branch branch
      <> " with "
      <> Pretty.branch upstream
      <> "."

syncStanza :: Sync -> Pretty
syncStanza sync =
  Pretty.indent 2 $
    Pretty.lines $
      fold
        [ ["│ " <> Pretty.style colorize (Pretty.branch sync.source <> " → " <> Pretty.branch sync.target)],
          map (\commit -> "│ " <> prettyGitCommitInfo commit) (Foldable.toList commits'),
          if more then ["│ ..."] else []
        ]
  where
    colorize :: Text.Builder -> Text.Builder
    colorize =
      if sync.success then Text.green else Text.red
    (commits', more) =
      case Seq1.length sync.commits > 10 of
        False -> (Seq1.toSeq sync.commits, False)
        True -> (Seq1.dropEnd 1 sync.commits, True)

------------------------------------------------------------------------------------------------------------------------
-- Rendering output to the terminal

renderOutput :: Output -> Pretty
renderOutput = \case
  Output.BranchAlreadyCheckedOut branch directory ->
    Pretty.line $
      Pretty.style Text.red $
        Pretty.branch branch
          <> " is already checked out in "
          <> Pretty.directory directory
          <> "."
  Output.CheckedOutBranch branch directory ->
    Pretty.line ("Checked out " <> Pretty.branch branch <> " in " <> Pretty.directory directory)
  Output.CreatedBranch branch directory maybeUpstream ->
    Pretty.line $
      "Created "
        <> Pretty.branch branch
        <> " in "
        <> Pretty.directory directory
        <> case maybeUpstream of
          Nothing -> ""
          Just upstream -> " tracking " <> Pretty.branch upstream
  Output.DirectoryAlreadyExists directory ->
    Pretty.line (Pretty.style Text.red ("Directory " <> Pretty.directory directory <> " already exists."))
  Output.GitTooOld -> Pretty.line (Pretty.style Text.red "Minimum required git version: 2.30.1")
  Output.MergeFailed source target commits ->
    Pretty.paragraphs
      [ (Pretty.branch source <> " was not merged into " <> Pretty.branch target <> ".")
          & Pretty.style Text.red
          & Pretty.line,
        syncStanza Sync {commits, success = False, source, target}
      ]
  Output.MergeSucceeded source target maybeCommits ->
    case maybeCommits of
      Nothing ->
        (Pretty.branch source <> " was merged into " <> Pretty.branch target <> ".")
          & Pretty.style Text.green
          & Pretty.line
      Just commits ->
        Pretty.paragraphs
          [ (Pretty.branch source <> " was merged into " <> Pretty.branch target <> ".")
              & Pretty.style Text.green
              & Pretty.line,
            Pretty.indent 2 (prettyCommits commits)
          ]
  Output.MergeInProgress -> Pretty.line (Pretty.style Text.red (Pretty.style Text.bold "git merge" <> " in progress."))
  Output.NoGitDir -> Pretty.line (Pretty.style Text.red "The current directory doesn't contain a git repository.")
  Output.NoSuchBranch -> Pretty.line (Pretty.style Text.red "No such branch.")
  Output.NotOnBranch -> Pretty.line (Pretty.style Text.red "You are not on a branch.")
  Output.NothingToCommit -> Pretty.line (Pretty.style Text.red "There's nothing to commit.")
  Output.NothingToMerge source target ->
    (Pretty.branch target <> " is already up-to-date with " <> Pretty.branch source <> ".")
      & Pretty.style Text.green
      & Pretty.line
  Output.NothingToUndo -> Pretty.line (Pretty.style Text.red "Nothing to undo.")
  Output.PushFailed commits ->
    Pretty.line (Pretty.style Text.red ("Tried to push " <> commitsN (Seq1.length commits) <> ", but failed."))
  Output.PushSucceeded commits ->
    Pretty.paragraphs
      [ Pretty.line (Pretty.style Text.green ("Pushed " <> commitsN (Seq1.length commits) <> ".")),
        Pretty.indent 2 (prettyCommits commits)
      ]
  Output.PushWouldBeRejected localCommits numRemoteCommits ->
    Pretty.line
      ( Pretty.style
          Text.red
          ( "Didn't try to push "
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
          ("Didn't try to push " <> commitsN (Seq1.length commits) <> ", because you appear to be offline.")
      )
  Output.UpstreamIsAhead branch upstream ->
    Pretty.paragraphs
      [ notSynchronizedStanza branch upstream ".",
        runSyncStanza "Run" branch upstream
      ]
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
      then List.map p (List.take 10 (Seq1.toList commits))
      else List.map p (List.take 8 (Seq1.toList commits)) ++ ["│ ...", p (Seq1.last commits)]
  where
    p = ("│ " <>) . prettyGitCommitInfo

prettyGitCommitInfo :: GitCommitInfo -> Pretty.Line
prettyGitCommitInfo info =
  Pretty.style (Text.Builder.bold . Text.Builder.black) (Pretty.text info.shorthash)
    <> Pretty.char ' '
    <> Pretty.style (Text.Builder.bold . Text.Builder.white) (Pretty.text info.subject)
    <> " - "
    <> Pretty.style (Text.Builder.italic . Text.Builder.white) (Pretty.text info.author)
    <> Pretty.char ' '
    <> Pretty.style (Text.Builder.italic . Text.Builder.yellow) (Pretty.text info.date)

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
