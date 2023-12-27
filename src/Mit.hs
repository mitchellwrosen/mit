module Mit
  ( main,
  )
where

import Control.Applicative (many)
import Data.Foldable qualified as Foldable (toList)
import Data.Functor.Contravariant ((>$<))
import Data.List.NonEmpty qualified as List1
import Data.Sequence qualified as Seq
import Data.Text.Builder.Linear qualified as Text (Builder)
import Mit.Command.Branch (mitBranch)
import Mit.Command.Status (mitStatus)
import Mit.Git
  ( DiffResult (Differences, NoDifferences),
    GitCommitInfo (hash),
    GitConflict,
    GitVersion (GitVersion),
    git,
    git2,
    gitApplyStash,
    gitBranchHead,
    gitCommitsBetween,
    gitConflicts,
    gitConflictsWith,
    gitDiff,
    gitExistCommitsBetween,
    gitFetch,
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
import Mit.Label
import Mit.Output (Output)
import Mit.Output qualified as Output
import Mit.Prelude
import Mit.Pretty (Pretty)
import Mit.Pretty qualified as Pretty
import Mit.Seq1 qualified as Seq1
import Mit.Snapshot (Snapshot, performSnapshot, snapshotHead, snapshotStash, undoToSnapshot, unsafeSnapshotHead)
import Mit.State (MitState (..), readMitState, writeMitState)
import Mit.Undo (Undo (..), applyUndo, undosStash)
import Mit.Verbosity (Verbosity, intToVerbosity)
import Options.Applicative qualified as Opt
import Options.Applicative.Types qualified as Opt (Backtracking (Backtrack))
import System.Exit (exitFailure)
import System.Posix.Terminal (queryTerminal)
import Text.Builder.ANSI qualified as Text

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
  (verbosity, command) <- Opt.customExecParser parserPrefs parserInfo
  main1 verbosity command & onLeftM \err -> do
    output (renderOutput err)
    exitFailure
  where
    parserPrefs :: Opt.ParserPrefs
    parserPrefs =
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

    parserInfo :: Opt.ParserInfo (Verbosity, MitCommand)
    parserInfo =
      Opt.info parser $
        Opt.progDesc "mit: a git wrapper with a streamlined UX"

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
            Opt.command "gc" $
              Opt.info
                (pure MitCommand'Gc)
                (Opt.progDesc "Delete stale, merged branches."),
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

main1 :: Verbosity -> MitCommand -> IO (Either Output ())
main1 verbosity command =
  label \return ->
    stick (Left >$< return) (Right <$> main2 verbosity command)

main2 :: (Abort Output) => Verbosity -> MitCommand -> IO ()
main2 verbosity command = do
  version <- gitVersion verbosity

  -- 'git stash create' broken before 2.30.1
  when (version < GitVersion 2 30 1) (abort Output.GitTooOld)
  whenNotM (gitRevParseAbsoluteGitDir verbosity) (abort Output.NoGitDir)

  case command of
    MitCommand'Branch branch -> mitBranch (output . renderOutput) verbosity branch
    MitCommand'Commit allFlag maybeMessage -> mitCommit verbosity allFlag maybeMessage
    MitCommand'Gc -> mitGc verbosity
    MitCommand'Merge branch -> mitMerge verbosity branch
    MitCommand'Status -> mitStatus verbosity
    MitCommand'Sync -> mitSync verbosity
    MitCommand'Undo -> mitUndo verbosity

data MitCommand
  = MitCommand'Branch Text
  | MitCommand'Commit Bool {- --all? -} (Maybe Text {- message -})
  | MitCommand'Gc
  | MitCommand'Merge Text
  | MitCommand'Status
  | MitCommand'Sync
  | MitCommand'Undo

dieIfMergeInProgress :: (Abort Output) => Verbosity -> IO ()
dieIfMergeInProgress verbosity =
  whenM (gitMergeInProgress verbosity) (abort Output.MergeInProgress)

mitCommit :: (Abort Output) => Verbosity -> Bool -> Maybe Text -> IO ()
mitCommit verbosity allFlag maybeMessage = do
  gitMergeInProgress verbosity >>= \case
    False -> mitCommitNotMerge verbosity allFlag maybeMessage
    True -> mitCommitMerge verbosity

mitCommitNotMerge :: (Abort Output) => Verbosity -> Bool -> Maybe Text -> IO ()
mitCommitNotMerge verbosity allFlag maybeMessage = do
  gitUnstageChanges verbosity
  gitDiff verbosity >>= \case
    Differences -> pure ()
    NoDifferences -> abort Output.NothingToCommit

  context <- getContext verbosity
  let upstream = contextUpstream context

  abortIfRemoteIsAhead verbosity context

  committed <- do
    doCommitAll <-
      if allFlag
        then pure True
        else not <$> queryTerminal 0
    case (doCommitAll, maybeMessage) of
      (True, Nothing) -> git2 verbosity ["commit", "--all"]
      (True, Just message) -> git verbosity ["commit", "--all", "--message", message]
      (False, Nothing) -> git2 verbosity ["commit", "--patch", "--quiet"]
      (False, Just message) -> git2 verbosity ["commit", "--patch", "--message", message, "--quiet"]

  push <- performPush verbosity context.branch
  undoPush <-
    case push of
      Pushed commits ->
        case Seq1.toList commits of
          [commit] ->
            gitIsMergeCommit verbosity commit.hash <&> \case
              False -> [Revert commit.hash]
              True -> []
          _ -> pure []
      _ -> pure []

  let state =
        MitState
          { head = (),
            merging = Nothing,
            undos =
              case (pushPushed push, committed) of
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

  writeMitState verbosity context.branch state

  remoteCommits <-
    case context.upstreamHead of
      Nothing -> pure Seq.empty
      Just upstreamHead -> gitCommitsBetween verbosity (Just "HEAD") upstreamHead

  conflictsOnSync <-
    if Seq.null remoteCommits
      then pure []
      else gitConflictsWith verbosity (fromJust context.upstreamHead)

  output $
    Pretty.paragraphs
      [ isSynchronizedStanza context.branch push,
        Pretty.whenJust (Seq1.fromSeq remoteCommits) \commits ->
          syncStanza Sync {commits, success = False, source = upstream, target = context.branch},
        Pretty.whenJust (pushCommits push) \commits ->
          syncStanza Sync {commits, success = pushPushed push, source = context.branch, target = upstream},
        case push of
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

mitCommitMerge :: (Abort Output) => Verbosity -> IO ()
mitCommitMerge verbosity = do
  context <- getContext verbosity
  let upstream = contextUpstream context

  -- Make the merge commit. Commonly we'll have gotten here by `mit merge <branch>`, so we'll have a `state0.merging`
  -- that tells us we're merging in <branch>. But we also handle the case that we went `git merge` -> `mit commit`,
  -- because why not.
  case context.state.merging of
    Nothing -> git_ verbosity ["commit", "--all", "--no-edit"]
    Just merging ->
      let message = fold ["⅄ ", if merging == context.branch then "" else merging <> " → ", context.branch]
       in git_ verbosity ["commit", "--all", "--message", message]

  writeMitState verbosity context.branch context.state {merging = Nothing}

  let pretty0 = do
        fromMaybe Pretty.empty do
          merging <- context.state.merging
          -- FIXME remember why this guard is here and document it
          guard (merging /= context.branch)
          pure (mergeStanzas context.branch merging (Right Nothing))

  -- Three possible cases:
  --   1. We had a dirty working directory before `mit merge` (evidence: our undo has a `git stash apply` in it)
  --     a. We can cleanly unstash it, so proceed to sync
  --     b. We cannot cleanly unstash it, so don't sync, because that may *further* conflict, and we don't want nasty
  --        double conflict markers
  --   2. We had a clean working directory before `mit merge`, so proceed to sync

  case undosStash context.state.undos of
    Nothing -> mitSyncWith verbosity pretty0 (Just [Reset (unsafeSnapshotHead context.snapshot)])
    Just stash -> do
      conflicts <- gitApplyStash verbosity stash
      case List1.nonEmpty conflicts of
        -- FIXME we just unstashed, now we're about to stash again :/
        Nothing -> mitSyncWith verbosity pretty0 (Just [Reset (unsafeSnapshotHead context.snapshot), Apply stash])
        Just conflicts1 ->
          output $
            Pretty.paragraphs
              [ pretty0,
                conflictsStanza "These files are in conflict:" conflicts1,
                Pretty.line $
                  "Resolve the conflicts, then run "
                    <> Pretty.command "mit commit"
                    <> " to synchronize "
                    <> Pretty.branch context.branch
                    <> " with "
                    <> Pretty.branch upstream
                    <> ".",
                Pretty.when (not (null context.state.undos)) canUndoStanza
              ]

mitGc :: (Abort Output) => Verbosity -> IO ()
mitGc verbosity = do
  context <- getContext verbosity
  let _upstream = contextUpstream context
  pure ()

mitMerge :: (Abort Output) => Verbosity -> Text -> IO ()
mitMerge verbosity target = do
  dieIfMergeInProgress verbosity

  context <- getContext verbosity
  let upstream = contextUpstream context

  if target == context.branch || target == upstream
    then -- If on branch `foo`, treat `mit merge foo` and `mit merge origin/foo` as `mit sync`
      mitSyncWith verbosity Pretty.empty Nothing
    else do
      -- When given 'mit merge foo', prefer running 'git merge origin/foo' over 'git merge foo'
      targetCommit <-
        gitRemoteBranchHead verbosity "origin" target
          & onNothingM do
            gitBranchHead verbosity target
              & onNothingM (abort Output.NoSuchBranch)

      abortIfRemoteIsAhead verbosity context

      cleanWorkingTree verbosity context

      merge <- performMerge verbosity ("⅄ " <> target <> " → " <> context.branch) targetCommit

      stashConflicts <-
        case (mergeDidntFail merge, snapshotStash context.snapshot) of
          (True, Just stash) -> gitApplyStash verbosity stash
          _ -> pure []

      push <- performPush verbosity context.branch

      let state =
            MitState
              { head = (),
                merging =
                  if mergeDidntFail merge
                    then Nothing
                    else Just target,
                undos =
                  if pushPushed push || mergeDidntFail merge
                    then []
                    else undoToSnapshot context.snapshot
              }

      writeMitState verbosity context.branch state

      remoteCommits <-
        case context.upstreamHead of
          Nothing -> pure Seq.empty
          Just upstreamHead -> gitCommitsBetween verbosity (Just "HEAD") upstreamHead

      conflictsOnSync <-
        if Seq.null remoteCommits
          then pure []
          else gitConflictsWith verbosity (fromJust context.upstreamHead)

      output $
        Pretty.paragraphs
          [ Pretty.whenJust (mergeCommits merge) \commits ->
              mergeStanzas
                context.branch
                target
                if mergeDidntFail merge
                  then Right (Just commits)
                  else Left commits,
            isSynchronizedStanza context.branch push,
            Pretty.whenJust (Seq1.fromSeq remoteCommits) \commits ->
              syncStanza Sync {commits, success = False, source = upstream, target = context.branch},
            -- TODO show commits to remote
            case (Seq1.toList1 <$> mergeConflicts merge) <|> List1.nonEmpty stashConflicts of
              Nothing -> Pretty.empty
              Just conflicts1 -> conflictsStanza "These files are in conflict:" conflicts1,
            -- TODO audit this
            case push of
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
mitSync :: (Abort Output) => Verbosity -> IO ()
mitSync verbosity = do
  dieIfMergeInProgress verbosity
  mitSyncWith verbosity Pretty.empty Nothing

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
mitSyncWith :: (Abort Output) => Verbosity -> Pretty -> Maybe [Undo] -> IO ()
mitSyncWith verbosity pretty0 maybeUndos = do
  context <- getContext verbosity
  let upstream = contextUpstream context

  cleanWorkingTree verbosity context

  merge <-
    case context.upstreamHead of
      -- Yay: no upstream branch is not different from an up-to-date local branch
      Nothing -> pure NothingToMerge
      Just upstreamHead -> performMerge verbosity ("⅄ " <> context.branch) upstreamHead

  stashConflicts <-
    case (mergeDidntFail merge, snapshotStash context.snapshot) of
      (True, Just stash) -> gitApplyStash verbosity stash
      _ -> pure []

  push <- performPush verbosity context.branch

  let state =
        MitState
          { head = (),
            merging =
              if mergeDidntFail merge
                then Nothing
                else Just context.branch,
            undos =
              if pushPushed push
                then []
                else case maybeUndos of
                  Nothing ->
                    if mergeDidntFail merge
                      then []
                      else undoToSnapshot context.snapshot
                  -- FIXME hm, could consider appending those undos instead, even if they obviate the recent stash/merge
                  -- undos
                  Just undos' -> undos'
          }

  writeMitState verbosity context.branch state

  output $
    Pretty.paragraphs
      [ pretty0,
        isSynchronizedStanza context.branch push,
        Pretty.whenJust (mergeCommits merge) \commits ->
          syncStanza
            Sync
              { commits,
                success = mergeDidntFail merge,
                source = upstream,
                target = context.branch
              },
        Pretty.whenJust (pushCommits push) \commits ->
          syncStanza
            Sync
              { commits,
                success = pushPushed push,
                source = context.branch,
                target = upstream
              },
        Pretty.whenJust ((Seq1.toList1 <$> mergeConflicts merge) <|> List1.nonEmpty stashConflicts) \conflicts1 ->
          conflictsStanza "These files are in conflict:" conflicts1,
        case push of
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

-- FIXME output what we just undid
mitUndo :: (Abort Output) => Verbosity -> IO ()
mitUndo verbosity = do
  context <- getContext verbosity
  undos <-
    List1.nonEmpty context.state.undos
      & onNothing (abort Output.NothingToUndo)
  for_ undos (applyUndo verbosity)
  head <- git verbosity ["rev-parse", "HEAD"]
  -- It's impossible for the snapshot to have a Nothing head (empty repo) if we got this far, since we had undos
  when (head /= unsafeSnapshotHead context.snapshot) (mitSync verbosity)

-- If origin/branch is ahead of branch, abort.
abortIfRemoteIsAhead :: (Abort Output) => Verbosity -> Context -> IO ()
abortIfRemoteIsAhead verbosity context = do
  existRemoteCommits <- contextExistRemoteCommits verbosity context
  existLocalCommits <- contextExistLocalCommits verbosity context

  when (existRemoteCommits && not existLocalCommits) do
    abort (Output.RemoteIsAhead context.branch (contextUpstream context))

-- Clean the working tree, if it's dirty (it's been stashed).
cleanWorkingTree :: Verbosity -> Context -> IO ()
cleanWorkingTree verbosity context = do
  whenJust (snapshotStash context.snapshot) \_stash ->
    git_ verbosity ["reset", "--hard", "--quiet", "HEAD"]

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

isSynchronizedStanza :: Text -> GitPush -> Pretty
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

getContext :: (Abort Output) => Verbosity -> IO Context
getContext verbosity = do
  gitFetch_ verbosity "origin"
  branch <- git verbosity ["branch", "--show-current"] & onNothingM (abort Output.NotOnBranch)
  upstreamHead <- gitRemoteBranchHead verbosity "origin" branch
  state <- readMitState verbosity branch
  snapshot <- performSnapshot verbosity
  pure Context {branch, snapshot, state, upstreamHead}

contextExistLocalCommits :: Verbosity -> Context -> IO Bool
contextExistLocalCommits verbosity context = do
  case context.upstreamHead of
    Nothing -> pure True
    Just upstreamHead ->
      case snapshotHead context.snapshot of
        Nothing -> pure False
        Just head -> gitExistCommitsBetween verbosity upstreamHead head

contextExistRemoteCommits :: Verbosity -> Context -> IO Bool
contextExistRemoteCommits verbosity context = do
  case context.upstreamHead of
    Nothing -> pure False
    Just upstreamHead ->
      case snapshotHead context.snapshot of
        Nothing -> pure True
        Just head -> gitExistCommitsBetween verbosity head upstreamHead

contextUpstream :: Context -> Text
contextUpstream context =
  "origin/" <> context.branch

------------------------------------------------------------------------------------------------------------------------
-- Git merge

-- | The result of a @git merge@. Lists of commits do not contain the merge commit itself.
data GitMerge
  = NothingToMerge
  | TriedToMerge (Seq1 GitCommitInfo) (Seq1 GitConflict)
  | Merged (Seq1 GitCommitInfo) -- note: doesn't distinguish between FF and non-FF

mergeCommits :: GitMerge -> Maybe (Seq1 GitCommitInfo)
mergeCommits = \case
  NothingToMerge -> Nothing
  TriedToMerge commits _conflicts -> Just commits
  Merged commits -> Just commits

mergeConflicts :: GitMerge -> Maybe (Seq1 GitConflict)
mergeConflicts = \case
  NothingToMerge -> Nothing
  TriedToMerge _commits conflicts -> Just conflicts
  Merged _commits -> Nothing

mergeDidntFail :: GitMerge -> Bool
mergeDidntFail =
  isNothing . mergeConflicts

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

-- Perform a fast-forward-if-possible git merge.
performMerge :: Verbosity -> Text -> Text -> IO GitMerge
performMerge verbosity message commitish = do
  head <- git verbosity ["rev-parse", "HEAD"]
  commits0 <- gitCommitsBetween verbosity (Just head) commitish
  case Seq1.fromSeq commits0 of
    Nothing -> pure NothingToMerge
    Just commits -> do
      git verbosity ["merge", "--ff", "--no-commit", commitish] >>= \case
        False -> do
          conflicts <- gitConflicts verbosity
          pure (TriedToMerge commits (Seq1.unsafeFromList conflicts))
        True -> do
          -- If this was a fast-forward, a merge would not be in progress at this point.
          whenM (gitMergeInProgress verbosity) (git_ verbosity ["commit", "--message", message])
          pure (Merged commits)

------------------------------------------------------------------------------------------------------------------------
-- Git push

-- | The result of (considering a) git push.
data GitPush
  = -- | We didn't push anthing.
    DidntPush !DidntPushReason
  | -- | We successfully pushed commits.
    Pushed !(Seq1 GitCommitInfo)

data DidntPushReason
  = -- | There was nothing to push.
    NothingToPush
  | -- | We have commits to push, but we appear to be offline.
    PushWouldntReachRemote (Seq1 GitCommitInfo)
  | -- | We have commits to push, but there are also remote commits to merge.
    PushWouldBeRejected (Seq1 GitCommitInfo)
  | -- | We had commits to push, and tried to push, but it failed.
    TriedToPush (Seq1 GitCommitInfo)

pushCommits :: GitPush -> Maybe (Seq1 GitCommitInfo)
pushCommits = \case
  DidntPush NothingToPush -> Nothing
  DidntPush (PushWouldntReachRemote commits) -> Just commits
  DidntPush (PushWouldBeRejected commits) -> Just commits
  DidntPush (TriedToPush commits) -> Just commits
  Pushed commits -> Just commits

pushPushed :: GitPush -> Bool
pushPushed = \case
  DidntPush _ -> False
  Pushed _ -> True

-- TODO get context
performPush :: Verbosity -> Text -> IO GitPush
performPush verbosity branch = do
  fetched <- gitFetch verbosity "origin"
  head <- git verbosity ["rev-parse", "HEAD"]
  upstreamHead <- gitRemoteBranchHead verbosity "origin" branch
  commits <- gitCommitsBetween verbosity upstreamHead head

  case Seq1.fromSeq commits of
    Nothing -> pure (DidntPush NothingToPush)
    Just commits1 -> do
      existRemoteCommits <-
        case upstreamHead of
          Nothing -> pure False
          Just upstreamHead1 -> gitExistCommitsBetween verbosity head upstreamHead1
      if existRemoteCommits
        then pure (DidntPush (PushWouldBeRejected commits1))
        else
          if fetched
            then do
              let args = ["push", "--follow-tags", "--set-upstream", "origin", "--quiet", branch <> ":" <> branch]
              git verbosity args <&> \case
                False -> DidntPush (TriedToPush commits1)
                True -> Pushed commits1
            else pure (DidntPush (PushWouldntReachRemote commits1))

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
