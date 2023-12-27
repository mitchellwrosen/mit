module Mit
  ( main,
  )
where

import Control.Applicative (many)
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable (toList)
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
    GitCommitInfo (hash),
    GitConflict,
    GitVersion (GitVersion),
    git,
    git2,
    gitApplyStash,
    gitCommitsBetween,
    gitConflictsWith,
    gitCurrentBranch,
    gitDiff,
    gitExistCommitsBetween,
    gitFetch_,
    gitIsMergeCommit,
    gitMergeInProgress,
    gitRemoteBranchHead,
    gitRevParseAbsoluteGitDir,
    gitUnstageChanges,
    gitVersion,
    git_,
    prettyGitCommitInfo,
    prettyGitConflict,
  )
import Mit.Label (Abort, abort, label, stick)
import Mit.Logger (Logger, makeLogger)
import Mit.Merge (MergeResult (..), mergeResultCommits, mergeResultConflicts, performMerge)
import Mit.Output (Output)
import Mit.Output qualified as Output
import Mit.Prelude
import Mit.Pretty (Pretty)
import Mit.Pretty qualified as Pretty
import Mit.ProcessInfo (ProcessInfo (..))
import Mit.Push (DidntPushReason (NothingToPush, PushWouldBeRejected, PushWouldntReachRemote, TriedToPush), PushResult (DidntPush, Pushed), performPush, pushResultCommits, pushResultPushed)
import Mit.Seq1 qualified as Seq1
import Mit.Snapshot (Snapshot, performSnapshot, snapshotHead, snapshotStash, undoToSnapshot)
import Mit.State (MitState (..), readMitState, writeMitState, writeMitState2)
import Mit.Undo (Undo (..), undosStash)
import Mit.Verbosity (Verbosity (..), intToVerbosity)
import Options.Applicative qualified as Opt
import Options.Applicative.Types qualified as Opt (Backtracking (Backtrack))
import System.Exit (ExitCode (..), exitFailure)
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

  let processInfoLogger :: Logger ProcessInfo
      processInfoLogger =
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

  main1 processInfoLogger command & onLeftM \err -> do
    output (renderOutput err)
    exitFailure
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
                    <$> Opt.switch (Opt.help "All changes" <> Opt.long "all")
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

main1 :: Logger ProcessInfo -> MitCommand -> IO (Either Output ())
main1 logger command =
  label \return ->
    stick (Left >$< return) (Right <$> main2 logger command)

main2 :: (Abort Output) => Logger ProcessInfo -> MitCommand -> IO ()
main2 logger command = do
  version <- gitVersion logger
  when (version < GitVersion 2 30 1) (abort Output.GitTooOld) -- 'git stash create' broken before 2.30.1
  whenNotM (gitRevParseAbsoluteGitDir logger) (abort Output.NoGitDir)

  case command of
    MitCommand'Branch branch -> mitBranch (output . renderOutput) logger branch
    MitCommand'Commit allFlag maybeMessage -> mitCommit logger allFlag maybeMessage
    MitCommand'Gc -> mitGc logger
    MitCommand'Merge branch -> mitMerge logger branch
    MitCommand'Status -> mitStatus logger
    MitCommand'Sync -> mitSync logger
    MitCommand'Undo -> mitUndo logger mitSync

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

mitCommit :: (Abort Output) => Logger ProcessInfo -> Bool -> Maybe Text -> IO ()
mitCommit logger allFlag maybeMessage = do
  gitMergeInProgress logger >>= \case
    False -> mitCommitNotMerge logger allFlag maybeMessage
    True -> mitCommitMerge logger

mitCommitNotMerge :: (Abort Output) => Logger ProcessInfo -> Bool -> Maybe Text -> IO ()
mitCommitNotMerge logger allFlag maybeMessage = do
  gitUnstageChanges logger
  gitDiff logger >>= \case
    Differences -> pure ()
    NoDifferences -> abort Output.NothingToCommit

  context <- getContext logger
  let upstream = contextUpstream context

  abortIfRemoteIsAhead logger context

  committed <- do
    doCommitAll <-
      if allFlag
        then pure True
        else not <$> queryTerminal 0
    case (doCommitAll, maybeMessage) of
      (True, Nothing) -> git2 logger ["commit", "--all"]
      (True, Just message) -> git logger ["commit", "--all", "--message", message]
      (False, Nothing) -> git2 logger ["commit", "--patch", "--quiet"]
      (False, Just message) -> git2 logger ["commit", "--patch", "--message", message, "--quiet"]

  pushResult <- performPush logger context.branch
  undoPush <-
    case pushResult of
      Pushed commits ->
        case Seq1.toList commits of
          [commit] ->
            gitIsMergeCommit logger commit.hash <&> \case
              False -> [Revert commit.hash]
              True -> []
          _ -> pure []
      DidntPush _reason -> pure []

  let state =
        MitState
          { head = (),
            merging = Nothing,
            undos =
              case (pushResultPushed pushResult, committed) of
                (False, False) -> context.state.undos
                (False, True) -> undoToSnapshot context.snapshot
                (True, False) -> undoPush
                (True, True) ->
                  if null undoPush
                    then []
                    else case snapshotStash context.snapshot of
                      Nothing -> undoPush
                      Just stash -> undoPush ++ [Apply stash]
          }

  writeMitState logger context.branch state

  remoteCommits <-
    case context.upstreamHead of
      Nothing -> pure Seq.empty
      Just upstreamHead -> gitCommitsBetween logger (Just "HEAD") upstreamHead

  conflictsOnSync <-
    if Seq.null remoteCommits
      then pure []
      else gitConflictsWith logger (fromJust context.upstreamHead)

  output $
    Pretty.paragraphs
      [ isSynchronizedStanza context.branch pushResult,
        Pretty.whenJust (Seq1.fromSeq remoteCommits) \commits ->
          syncStanza Sync {commits, success = False, source = upstream, target = context.branch},
        Pretty.whenJust (pushResultCommits pushResult) \commits ->
          syncStanza Sync {commits, success = pushResultPushed pushResult, source = context.branch, target = upstream},
        case pushResult of
          DidntPush NothingToPush -> Pretty.empty
          DidntPush (PushWouldntReachRemote _) -> runSyncStanza "When you come online, run" context.branch upstream
          DidntPush (PushWouldBeRejected _) ->
            case List1.nonEmpty conflictsOnSync of
              Nothing -> runSyncStanza "Run" context.branch upstream
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
                        <> Pretty.branch context.branch
                        <> " with "
                        <> Pretty.branch upstream
                        <> "."
                  ]
          DidntPush (TriedToPush _) -> runSyncStanza "Run" context.branch upstream
          Pushed _ -> Pretty.empty,
        -- Whether we say we can undo from here is not exactly if the state says we can undo, because of one corner
        -- case: we ran 'mit commit', then aborted the commit, and ultimately didn't push any other local changes.
        --
        -- In this case, the underlying state hasn't changed, so 'mit undo' will still work as if the 'mit commit' was
        -- never run, we merely don't want to *say* "run 'mit undo' to undo" as feedback, because that sounds as if it
        -- would undo the last command run, namely the 'mit commit' that was aborted.
        Pretty.when (not (null state.undos) && committed) canUndoStanza
      ]

mitCommitMerge :: (Abort Output) => Logger ProcessInfo -> IO ()
mitCommitMerge logger = do
  branch <- gitCurrentBranch logger
  state <- readMitState logger branch
  head0 <- git @Text logger ["rev-parse", "HEAD"]

  -- Make the merge commit. Commonly we'll have gotten here by `mit merge <branch>`, so we'll have a `state0.merging`
  -- that tells us we're merging in <branch>. But we also handle the case that we went `git merge` -> `mit commit`,
  -- because why not.
  case state.merging of
    Nothing -> git_ logger ["commit", "--all", "--no-edit"]
    Just merging ->
      git_
        logger
        [ "commit",
          "--all",
          "--message",
          fold ["⅄ ", if merging == branch then "" else merging <> " → ", branch]
        ]

  head1 <- git logger ["rev-parse", "HEAD"]

  -- Record that we are no longer merging.
  writeMitState2 logger branch state {head = head1, merging = Nothing}

  let pretty0 = do
        fromMaybe Pretty.empty do
          merging <- state.merging
          -- FIXME remember why this guard is here and document it
          guard (merging /= branch)
          pure (mergeStanzas branch merging (Right Nothing))

  -- Three possible cases:
  --   1. We had a clean working directory before `mit merge`, so proceed to sync
  --   2. We had a dirty working directory before `mit merge` (evidence: our undo has a `git stash apply` in it)
  --     a. We can cleanly unstash it, so proceed to sync
  --     b. We cannot cleanly unstash it, so don't sync, because that may *further* conflict, and we don't want nasty
  --        double conflict markers

  case undosStash state.undos of
    Nothing -> mitSyncWith logger pretty0 (Just [Reset head0])
    Just stash -> do
      conflicts <- gitApplyStash logger stash
      case List1.nonEmpty conflicts of
        -- FIXME we just unstashed, now we're about to stash again :/
        Nothing -> mitSyncWith logger pretty0 (Just [Reset head0, Apply stash])
        Just conflicts1 ->
          output $
            Pretty.paragraphs
              [ pretty0,
                conflictsStanza "These files are in conflict:" conflicts1,
                Pretty.line $
                  "Resolve the conflicts, then run "
                    <> Pretty.command "mit commit"
                    <> " to synchronize "
                    <> Pretty.branch branch
                    <> " with "
                    <> Pretty.branch ("origin/" <> branch)
                    <> ".",
                Pretty.when (not (null state.undos)) canUndoStanza
              ]

mitGc :: (Abort Output) => Logger ProcessInfo -> IO ()
mitGc logger = do
  context <- getContext logger
  let _upstream = contextUpstream context
  pure ()

mitMerge :: (Abort Output) => Logger ProcessInfo -> Text -> IO ()
mitMerge logger target = do
  whenM (gitMergeInProgress logger) (abort Output.MergeInProgress)

  context <- getContext logger
  let upstream = contextUpstream context

  if target == context.branch || target == upstream
    then -- If on branch `foo`, treat `mit merge foo` and `mit merge origin/foo` as `mit sync`
      mitSyncWith logger Pretty.empty Nothing
    else do
      -- When given 'mit merge foo', prefer running 'git merge origin/foo' over 'git merge foo'
      targetCommit <-
        gitRemoteBranchHead logger "origin" target & onNothingM do
          git logger ["rev-parse", "refs/heads/" <> target] & onLeftM \_ ->
            abort Output.NoSuchBranch

      abortIfRemoteIsAhead logger context

      cleanWorkingTree logger context

      mergeResult <- performMerge logger targetCommit ("⅄ " <> target <> " → " <> context.branch)

      stashConflicts <-
        case (mergeResultConflicts mergeResult, snapshotStash context.snapshot) of
          (Nothing, Just stash) -> gitApplyStash logger stash
          _ -> pure []

      pushResult <- performPush logger context.branch

      let state =
            MitState
              { head = (),
                merging =
                  case mergeResultConflicts mergeResult of
                    Nothing -> Nothing
                    Just _conflicts -> Just target,
                undos =
                  if pushResultPushed pushResult || isNothing (mergeResultConflicts mergeResult)
                    then []
                    else undoToSnapshot context.snapshot
              }

      writeMitState logger context.branch state

      remoteCommits <-
        case context.upstreamHead of
          Nothing -> pure Seq.empty
          Just upstreamHead -> gitCommitsBetween logger (Just "HEAD") upstreamHead

      conflictsOnSync <-
        if Seq.null remoteCommits
          then pure []
          else gitConflictsWith logger (fromJust context.upstreamHead)

      output $
        Pretty.paragraphs
          [ Pretty.whenJust (mergeResultCommits mergeResult) \commits ->
              mergeStanzas
                context.branch
                target
                case mergeResultConflicts mergeResult of
                  Nothing -> Right (Just commits)
                  Just _conflicts -> Left commits,
            isSynchronizedStanza context.branch pushResult,
            Pretty.whenJust (Seq1.fromSeq remoteCommits) \commits ->
              syncStanza Sync {commits, success = False, source = upstream, target = context.branch},
            -- TODO show commits to remote
            case (Seq1.toList1 <$> mergeResultConflicts mergeResult) <|> List1.nonEmpty stashConflicts of
              Nothing -> Pretty.empty
              Just conflicts1 -> conflictsStanza "These files are in conflict:" conflicts1,
            -- TODO audit this
            case pushResult of
              DidntPush NothingToPush -> Pretty.empty
              DidntPush (PushWouldntReachRemote _) -> runSyncStanza "When you come online, run" context.branch upstream
              -- FIXME hrm, but we might have merge conflicts and/or stash conflicts!
              DidntPush (PushWouldBeRejected _) ->
                case List1.nonEmpty conflictsOnSync of
                  Nothing -> runSyncStanza "Run" context.branch upstream
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
                            <> Pretty.branch context.branch
                            <> " with "
                            <> Pretty.branch upstream
                            <> "."
                      ]
              DidntPush (TriedToPush _) -> runSyncStanza "Run" context.branch upstream
              Pushed _ -> Pretty.empty,
            Pretty.when (not (null state.undos)) canUndoStanza
          ]

-- TODO implement "lateral sync", i.e. a merge from some local or remote branch, followed by a sync to upstream
mitSync :: (Abort Output) => Logger ProcessInfo -> IO ()
mitSync logger = do
  whenM (gitMergeInProgress logger) (abort Output.MergeInProgress)
  mitSyncWith logger Pretty.empty Nothing

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
mitSyncWith :: (Abort Output) => Logger ProcessInfo -> Pretty -> Maybe [Undo] -> IO ()
mitSyncWith logger pretty0 maybeUndos = do
  context <- getContext logger
  let upstream = contextUpstream context

  cleanWorkingTree logger context

  mergeResult <-
    case context.upstreamHead of
      -- Yay: no upstream branch is not different from an up-to-date local branch
      Nothing -> pure NothingToMerge
      Just upstreamHead -> performMerge logger upstreamHead ("⅄ " <> context.branch)

  stashConflicts <-
    case (mergeResultConflicts mergeResult, snapshotStash context.snapshot) of
      (Nothing, Just stash) -> gitApplyStash logger stash
      _ -> pure []

  pushResult <- performPush logger context.branch

  let state =
        MitState
          { head = (),
            merging =
              case mergeResultConflicts mergeResult of
                Nothing -> Nothing
                Just _conflicts -> Just context.branch,
            undos =
              if pushResultPushed pushResult
                then []
                else case maybeUndos of
                  Nothing ->
                    case mergeResultConflicts mergeResult of
                      Nothing -> []
                      Just _conflicts -> undoToSnapshot context.snapshot
                  -- FIXME hm, could consider appending those undos instead, even if they obviate the recent stash/merge
                  -- undos
                  Just undos' -> undos'
          }

  writeMitState logger context.branch state

  output $
    Pretty.paragraphs
      [ pretty0,
        isSynchronizedStanza context.branch pushResult,
        Pretty.whenJust (mergeResultCommits mergeResult) \commits ->
          syncStanza
            Sync
              { commits,
                success =
                  case mergeResultConflicts mergeResult of
                    Nothing -> True
                    Just _conflicts -> False,
                source = upstream,
                target = context.branch
              },
        Pretty.whenJust (pushResultCommits pushResult) \commits ->
          syncStanza
            Sync
              { commits,
                success = pushResultPushed pushResult,
                source = context.branch,
                target = upstream
              },
        Pretty.whenJust ((Seq1.toList1 <$> mergeResultConflicts mergeResult) <|> List1.nonEmpty stashConflicts) \conflicts1 ->
          conflictsStanza "These files are in conflict:" conflicts1,
        case pushResult of
          DidntPush NothingToPush -> Pretty.empty
          DidntPush (PushWouldntReachRemote _) -> runSyncStanza "When you come online, run" context.branch upstream
          DidntPush (PushWouldBeRejected _) ->
            Pretty.line $
              "Resolve the conflicts, then run "
                <> Pretty.command "mit commit"
                <> " to synchronize "
                <> Pretty.branch context.branch
                <> " with "
                <> Pretty.branch upstream
                <> "."
          DidntPush (TriedToPush _) -> runSyncStanza "Run" context.branch upstream
          Pushed _ -> Pretty.empty,
        Pretty.when (not (null state.undos)) canUndoStanza
      ]

-- If origin/branch is ahead of branch, abort.
abortIfRemoteIsAhead :: (Abort Output) => Logger ProcessInfo -> Context -> IO ()
abortIfRemoteIsAhead logger context = do
  existRemoteCommits <- contextExistRemoteCommits logger context
  existLocalCommits <- contextExistLocalCommits logger context

  when (existRemoteCommits && not existLocalCommits) do
    abort (Output.RemoteIsAhead context.branch (contextUpstream context))

-- Clean the working tree, if it's dirty (it's been stashed).
cleanWorkingTree :: Logger ProcessInfo -> Context -> IO ()
cleanWorkingTree logger context = do
  whenJust (snapshotStash context.snapshot) \_stash ->
    git_ logger ["reset", "--hard", "--quiet", "HEAD"]

output :: Pretty -> IO ()
output p =
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
  DidntPush (PushWouldBeRejected _) -> notSynchronizedStanza branch upstream " (their histories have diverged)"
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
-- Context

data Context = Context
  { branch :: !Text,
    snapshot :: !Snapshot,
    state :: !(MitState ()),
    upstreamHead :: !(Maybe Text)
  }

getContext :: (Abort Output) => Logger ProcessInfo -> IO Context
getContext logger = do
  gitFetch_ logger "origin"
  branch <- gitCurrentBranch logger
  upstreamHead <- gitRemoteBranchHead logger "origin" branch
  state <- readMitState logger branch
  snapshot <- performSnapshot logger
  pure Context {branch, snapshot, state, upstreamHead}

contextExistLocalCommits :: Logger ProcessInfo -> Context -> IO Bool
contextExistLocalCommits logger context = do
  case context.upstreamHead of
    Nothing -> pure True
    Just upstreamHead ->
      case snapshotHead context.snapshot of
        Nothing -> pure False
        Just head -> gitExistCommitsBetween logger upstreamHead head

contextExistRemoteCommits :: Logger ProcessInfo -> Context -> IO Bool
contextExistRemoteCommits logger context = do
  case context.upstreamHead of
    Nothing -> pure False
    Just upstreamHead ->
      case snapshotHead context.snapshot of
        Nothing -> pure True
        Just head -> gitExistCommitsBetween logger head upstreamHead

contextUpstream :: Context -> Text
contextUpstream context =
  "origin/" <> context.branch

------------------------------------------------------------------------------------------------------------------------
-- Git merge

-- Left = merge failed, here are commits
-- Right Nothing = merge succeeded, but we don't remember commits
-- Right Just = merge succeeded, here are commits
-- FIXME persist the commits that we're merging so we can report them (no more Just Nothing case)
mergeStanzas :: Text -> Text -> Either (Seq1 GitCommitInfo) (Maybe (Seq1 GitCommitInfo)) -> Pretty
mergeStanzas branch other = \case
  Left commits ->
    Pretty.paragraphs
      [ (Pretty.branch other <> " was not merged into " <> Pretty.branch branch <> ".")
          & Pretty.style Text.red
          & Pretty.line,
        syncStanza
          Sync
            { commits,
              success = False,
              source = other,
              target = branch
            }
      ]
  Right Nothing ->
    (Pretty.branch other <> " was merged into " <> Pretty.branch branch <> ".")
      & Pretty.style Text.green
      & Pretty.line
  Right (Just commits) ->
    Pretty.paragraphs
      [ (Pretty.branch other <> " was merged into " <> Pretty.branch branch <> ".")
          & Pretty.style Text.green
          & Pretty.line,
        syncStanza
          Sync
            { commits,
              success = True,
              source = other,
              target = branch
            }
      ]

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
  Output.MergeInProgress -> Pretty.line (Pretty.style Text.red (Pretty.style Text.bold "git merge" <> " in progress."))
  Output.NoGitDir -> Pretty.line (Pretty.style Text.red "The current directory doesn't contain a git repository.")
  Output.NoSuchBranch -> Pretty.line (Pretty.style Text.red "No such branch.")
  Output.NotOnBranch -> Pretty.line (Pretty.style Text.red "You are not on a branch.")
  Output.NothingToCommit -> Pretty.line (Pretty.style Text.red "There's nothing to commit.")
  Output.NothingToUndo -> Pretty.line (Pretty.style Text.red "Nothing to undo.")
  Output.RemoteIsAhead branch upstream ->
    Pretty.paragraphs
      [ notSynchronizedStanza branch upstream ".",
        runSyncStanza "Run" branch upstream
      ]
