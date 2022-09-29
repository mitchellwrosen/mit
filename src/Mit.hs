module Mit
  ( main,
  )
where

import Data.List.NonEmpty qualified as List1
import Data.Ord (clamp)
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Builder.ANSI qualified as Text
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Data.Text.Lazy.Builder qualified as Text.Builder
import Mit.Builder qualified as Builder
import Mit.Directory
import Mit.Env
import Mit.Git
import Mit.GitCommand qualified as Git
import Mit.Monad
import Mit.Prelude
import Mit.Seq1 qualified as Seq1
import Mit.Stanza
import Mit.State
import Mit.Undo
import Options.Applicative qualified as Opt
import Options.Applicative.Types qualified as Opt (Backtracking (Backtrack))
import System.Exit (exitFailure)

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

main :: IO ()
main = do
  (verbosity, command) <- Opt.customExecParser parserPrefs parserInfo

  let action2 :: Mit () [Stanza] ()
      action2 = do
        withEnv (\() -> Env {gitdir = "", verbosity}) gitRevParseAbsoluteGitDir >>= \case
          Nothing -> throw [Just (Text.red "The current directory doesn't contain a git repository.")]
          Just gitdir ->
            withEnv
              (\() -> Env {gitdir, verbosity})
              ( case command of
                  MitCommand'Branch branch -> mitBranch branch
                  MitCommand'Commit -> mitCommit
                  MitCommand'Merge branch -> mitMerge branch
                  MitCommand'Sync -> mitSync
                  MitCommand'Undo -> mitUndo
              )

  runMit () ([] <$ action2) >>= \case
    [] -> pure ()
    errs -> do
      putStanzas errs
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

    parserInfo :: Opt.ParserInfo (Int, MitCommand)
    parserInfo =
      Opt.info parser $
        Opt.progDesc "mit: a git wrapper with a streamlined UX"

    parser :: Opt.Parser (Int, MitCommand)
    parser =
      (,)
        <$> ( clamp (0, 2)
                <$> Opt.option Opt.auto (Opt.help "Verbosity" <> Opt.metavar "«n»" <> Opt.short 'v' <> Opt.value 0)
            )
        <*> (Opt.hsubparser . fold)
          [ Opt.command "branch" $
              Opt.info
                (MitCommand'Branch <$> Opt.strArgument (Opt.metavar "≪branch≫"))
                (Opt.progDesc "Create a new branch in a new worktree."),
            Opt.command "commit" $
              Opt.info
                (pure MitCommand'Commit)
                (Opt.progDesc "Create a commit interactively."),
            Opt.command "merge" $
              Opt.info
                (MitCommand'Merge <$> Opt.strArgument (Opt.metavar "≪branch≫"))
                (Opt.progDesc "Merge the given branch into the current branch."),
            Opt.command "sync" $
              Opt.info
                (pure MitCommand'Sync)
                (Opt.progDesc "Sync with the remote named `origin`."),
            Opt.command "undo" $
              Opt.info
                (pure MitCommand'Undo)
                (Opt.progDesc "Undo the last `mit` command (if possible).")
          ]

data MitCommand
  = MitCommand'Branch Text
  | MitCommand'Commit
  | MitCommand'Merge Text
  | MitCommand'Sync
  | MitCommand'Undo

dieIfBuggyGit :: Mit Env [Stanza] ()
dieIfBuggyGit = do
  version <- gitVersion
  let validate (ver, err) = if version < ver then ((ver, err) :) else id
  case foldr validate [] validations of
    [] -> pure ()
    errors ->
      throw $
        map
          ( \(ver, err) ->
              Just
                ( Text.red
                    ( "Prior to " <> Text.bold "git" <> " version "
                        <> Text.Builder.fromText (showGitVersion ver)
                        <> ", "
                        <> err
                    )
                )
          )
          errors
  where
    validations :: [(GitVersion, Text.Builder)]
    validations =
      [ ( GitVersion 2 29 0,
          Text.bold "git commit --patch"
            <> " was broken for new files added with "
            <> Text.bold "git add --intent-to-add"
            <> "."
        ),
        ( GitVersion 2 30 1,
          Text.bold "git stash create"
            <> " was broken for new files added with "
            <> Text.bold "git add --intent-to-add"
            <> "."
        )
      ]

dieIfMergeInProgress :: Mit Env [Stanza] ()
dieIfMergeInProgress =
  whenM gitMergeInProgress (throw [Just (Text.red (Text.bold "git merge" <> " in progress."))])

mitBranch :: Text -> Mit Env [Stanza] ()
mitBranch branch = do
  worktreeDir <- do
    rootdir <- gitRevParseShowToplevel
    pure (Text.dropWhileEnd (/= '/') rootdir <> branch)

  gitBranchWorktreeDir branch >>= \case
    Nothing -> do
      whenM (doesDirectoryExist worktreeDir) do
        throw [Just (Text.red ("Directory " <> Text.bold (Text.Builder.fromText worktreeDir) <> " already exists."))]
      git_ ["worktree", "add", "--detach", worktreeDir]
      block do
        cd worktreeDir
        whenNotM (Git.git (Git.Switch branch)) do
          gitBranch branch
          Git.git_ (Git.Switch branch)
          gitFetch_ "origin"
          whenM (gitRemoteBranchExists "origin" branch) do
            let upstream = "origin/" <> branch
            Git.git_ (Git.Reset Git.Hard Git.FlagQuiet upstream)
            Git.git_ (Git.BranchSetUpstreamTo upstream)
    Just directory ->
      when (directory /= worktreeDir) do
        throw
          [ Just
              ( Text.red
                  ( Text.bold
                      ( Text.Builder.fromText branch
                          <> " is already checked out in "
                          <> Text.bold (Text.Builder.fromText directory)
                      )
                      <> "."
                  )
              )
          ]

mitCommit :: Mit Env [Stanza] ()
mitCommit = do
  whenM gitExistUntrackedFiles dieIfBuggyGit

  gitMergeInProgress >>= \case
    False ->
      gitDiff >>= \case
        Differences -> mitCommit_
        NoDifferences -> throw [Just (Text.red "There's nothing to commit.")]
    True -> mitCommitMerge

mitCommit_ :: Mit Env [Stanza] ()
mitCommit_ = do
  context <- getContext
  let upstream = "origin/" <> context.branch

  existRemoteCommits <- contextExistRemoteCommits context
  existLocalCommits <- contextExistLocalCommits context

  when (existRemoteCommits && not existLocalCommits) do
    throw
      [ notSynchronizedStanza context.branch upstream ".",
        runSyncStanza "Run" context.branch upstream
      ]

  committed <- gitCommit

  push <- performPush context.branch

  let state =
        MitState
          { head = (),
            merging = Nothing,
            undos =
              case (pushPushed push, committed) of
                (False, False) -> context.state.undos
                (False, True) -> undoToSnapshot context.snapshot
                (True, False) -> push.undo
                (True, True) ->
                  if null push.undo
                    then []
                    else push.undo ++ [Apply (fromJust context.snapshot.stash)]
          }

  writeMitState context.branch state

  remoteCommits <-
    case context.upstreamHead of
      Nothing -> pure Seq.empty
      Just upstreamHead -> gitCommitsBetween (Just "HEAD") upstreamHead

  conflictsOnSync <-
    if Seq.null remoteCommits
      then pure []
      else gitConflictsWith (fromJust context.upstreamHead)

  io do
    putStanzas
      [ isSynchronizedStanza2 context.branch push.what,
        do
          commits <- Seq1.fromSeq remoteCommits
          syncStanza Sync {commits, success = False, source = upstream, target = context.branch},
        do
          commits <- Seq1.fromSeq push.commits
          syncStanza Sync {commits, success = pushPushed push, source = context.branch, target = upstream},
        case push.what of
          NothingToPush2 -> Nothing
          Pushed -> Nothing
          PushWouldntReachRemote -> runSyncStanza "When you come online, run" context.branch upstream
          PushWouldBeRejected ->
            case List1.nonEmpty conflictsOnSync of
              Nothing -> runSyncStanza "Run" context.branch upstream
              Just conflictsOnSync1 ->
                renderStanzas
                  [ conflictsStanza
                      ( "These files will be in conflict when you run "
                          <> Text.bold (Text.blue "mit sync")
                          <> ":"
                      )
                      conflictsOnSync1,
                    Just $
                      "  Run "
                        <> Text.bold (Text.blue "mit sync")
                        <> ", resolve the conflicts, then run "
                        <> Text.bold (Text.blue "mit commit")
                        <> " to synchronize "
                        <> branchb context.branch
                        <> " with "
                        <> branchb upstream
                        <> "."
                  ]
          TriedToPush -> runSyncStanza "Run" context.branch upstream,
        -- Whether we say we can undo from here is not exactly if the state says we can undo, because of one corner case:
        -- we ran 'mit commit', then aborted the commit, and ultimately didn't push any other local changes.
        --
        -- In this case, the underlying state hasn't changed, so 'mit undo' will still work as if the 'mit commit' was
        -- never run, we merely don't want to *say* "run 'mit undo' to undo" as feedback, because that sounds as if it
        -- would undo the last command run, namely the 'mit commit' that was aborted.
        if not (null state.undos) && committed then canUndoStanza else Nothing
      ]

mitCommitMerge :: Mit Env x ()
mitCommitMerge = do
  context <- getContext

  -- Make the merge commit. Commonly we'll have gotten here by `mit merge <branch>`, so we'll have a `state0.merging`
  -- that tells us we're merging in <branch>. But we also handle the case that we went `git merge` -> `mit commit`,
  -- because why not.
  case context.state.merging of
    Nothing -> git_ ["commit", "--all", "--no-edit"]
    Just merging ->
      let message = fold ["⅄ ", if merging == context.branch then "" else merging <> " → ", context.branch]
       in git_ ["commit", "--all", "--message", message]

  writeMitState context.branch context.state {merging = Nothing}

  let stanza0 = do
        merging <- context.state.merging
        guard (merging /= context.branch)
        synchronizedStanza context.branch merging

  -- Three possible cases:
  --   1. We had a dirty working directory before `mit merge` (evidence: our undo has a `git stash apply` in it)
  --     a. We can cleanly unstash it, so proceed to sync
  --     b. We cannot cleanly unstash it, so don't sync, because that may *further* conflict, and we don't want nasty
  --        double conflict markers
  --   2. We had a clean working directory before `mit merge`, so proceed to sync

  case undosStash context.state.undos of
    Nothing -> mitSyncWith stanza0 (Just [Reset context.snapshot.head])
    Just stash -> do
      conflicts <- gitApplyStash stash
      case List1.nonEmpty conflicts of
        -- FIXME we just unstashed, now we're about to stash again :/
        Nothing -> mitSyncWith stanza0 (Just [Reset context.snapshot.head, Apply stash])
        Just conflicts1 ->
          io do
            putStanzas
              [ stanza0,
                conflictsStanza "These files are in conflict:" conflicts1,
                -- Fake like we didn't push due to merge conflicts just to print "resolve conflicts and commit"
                whatNextStanza context.branch (PushNotAttempted MergeConflicts),
                if null context.state.undos then Nothing else canUndoStanza
              ]

-- FIXME delete
data PushResult
  = PushAttempted Bool
  | PushNotAttempted PushNotAttemptedReason

data PushNotAttemptedReason
  = ForkedHistory [GitConflict] -- local history has forked, need to sync.
  | MergeConflicts -- local merge conflicts that need to be resolved right now
  | NothingToPush -- no commits to push
  | Offline -- fetch failed, so we seem offline
  | UnseenCommits -- we just pulled remote commits; don't push in case there's something local to address

mitMerge :: Text -> Mit Env [Stanza] ()
mitMerge target = do
  dieIfMergeInProgress
  whenM gitExistUntrackedFiles dieIfBuggyGit

  context <- getContext
  let upstream = "origin/" <> context.branch

  if target == context.branch || target == upstream
    then -- If on branch `foo`, treat `mit merge foo` and `mit merge origin/foo` as `mit sync`
      mitSyncWith Nothing Nothing
    else mitMergeWith context target

mitMergeWith :: Context -> Text -> Mit Env [Stanza] ()
mitMergeWith context target = do
  -- When given 'mit merge foo', prefer running 'git merge origin/foo' over 'git merge foo'
  targetCommit <-
    gitRemoteBranchHead "origin" target
      & onNothingM
        ( gitBranchHead target
            & onNothingM (throw [Just (Text.red "No such branch.")])
        )

  let upstream = "origin/" <> context.branch

  existRemoteCommits <- contextExistRemoteCommits context
  existLocalCommits <- contextExistLocalCommits context

  when (existRemoteCommits && not existLocalCommits) do
    throw
      [ notSynchronizedStanza context.branch upstream ".",
        runSyncStanza "Run" context.branch upstream
      ]

  whenJust context.snapshot.stash \_stash ->
    Git.git_ (Git.Reset Git.Hard Git.FlagQuiet "HEAD")

  merge <- performMerge ("⅄ " <> target <> " → " <> context.branch) targetCommit

  stashConflicts <-
    if null merge.conflicts
      then case context.snapshot.stash of
        Nothing -> pure []
        Just stash -> gitApplyStash stash
      else pure []

  push <- performPush context.branch

  let state =
        MitState
          { head = (),
            merging =
              if null merge.conflicts
                then Nothing
                else Just target,
            undos =
              if pushPushed push || Seq.null merge.commits
                then []
                else undoToSnapshot context.snapshot
          }

  writeMitState context.branch state

  remoteCommits <-
    case context.upstreamHead of
      Nothing -> pure Seq.empty
      Just upstreamHead -> gitCommitsBetween (Just "HEAD") upstreamHead

  conflictsOnSync <-
    if Seq.null remoteCommits
      then pure []
      else gitConflictsWith (fromJust context.upstreamHead)

  io do
    putStanzas
      [ if null merge.conflicts
          then synchronizedStanza context.branch target
          else notSynchronizedStanza context.branch target ".",
        do
          commits1 <- Seq1.fromSeq merge.commits
          syncStanza
            Sync
              { commits = commits1,
                success = null merge.conflicts,
                source = target,
                target = context.branch
              },
        isSynchronizedStanza2 context.branch push.what,
        do
          commits <- Seq1.fromSeq remoteCommits
          syncStanza Sync {commits, success = False, source = upstream, target = context.branch},
        -- TODO show commits to remote
        do
          conflicts1 <- List1.nonEmpty merge.conflicts <|> List1.nonEmpty stashConflicts
          conflictsStanza "These files are in conflict:" conflicts1,
        -- TODO audit this
        case push.what of
          NothingToPush2 -> Nothing
          Pushed -> Nothing
          PushWouldntReachRemote -> runSyncStanza "When you come online, run" context.branch upstream
          -- FIXME hrm, but we might have merge conflicts and/or stash conflicts!
          PushWouldBeRejected ->
            case List1.nonEmpty conflictsOnSync of
              Nothing -> runSyncStanza "Run" context.branch upstream
              Just conflictsOnSync1 ->
                renderStanzas
                  [ conflictsStanza
                      ( "These files will be in conflict when you run "
                          <> Text.bold (Text.blue "mit sync")
                          <> ":"
                      )
                      conflictsOnSync1,
                    Just $
                      "  Run "
                        <> Text.bold (Text.blue "mit sync")
                        <> ", resolve the conflicts, then run "
                        <> Text.bold (Text.blue "mit commit")
                        <> " to synchronize "
                        <> branchb context.branch
                        <> " with "
                        <> branchb upstream
                        <> "."
                  ]
          TriedToPush -> runSyncStanza "Run" context.branch upstream,
        if not (null state.undos) then canUndoStanza else Nothing
      ]

-- TODO implement "lateral sync", i.e. a merge from some local or remote branch, followed by a sync to upstream
mitSync :: Mit Env [Stanza] ()
mitSync = do
  dieIfMergeInProgress
  whenM gitExistUntrackedFiles dieIfBuggyGit
  mitSyncWith Nothing Nothing

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
mitSyncWith :: Stanza -> Maybe [Undo] -> Mit Env x ()
mitSyncWith stanza0 maybeUndos = do
  context <- getContext
  let upstream = "origin/" <> context.branch

  whenJust context.snapshot.stash \_stash ->
    Git.git_ (Git.Reset Git.Hard Git.FlagQuiet "HEAD")

  merge <-
    case context.upstreamHead of
      -- Yay: no upstream branch is not different from an up-to-date local branch
      Nothing -> pure GitMerge {commits = Seq.empty, conflicts = []}
      Just upstreamHead -> performMerge ("⅄ " <> context.branch) upstreamHead

  stashConflicts <-
    if null merge.conflicts
      then case context.snapshot.stash of
        Nothing -> pure []
        Just stash -> gitApplyStash stash
      else pure []

  push <- performPush context.branch

  let state =
        MitState
          { head = (),
            merging =
              if null merge.conflicts
                then Nothing
                else Just context.branch,
            undos =
              if pushPushed push
                then []
                else case maybeUndos of
                  Nothing ->
                    if Seq.null merge.commits
                      then []
                      else undoToSnapshot context.snapshot
                  -- FIXME hm, could consider appending those undos instead, even if they obviate the recent stash/merge
                  -- undos
                  Just undos' -> undos'
          }

  writeMitState context.branch state

  io do
    putStanzas
      [ stanza0,
        isSynchronizedStanza2 context.branch push.what,
        do
          commits1 <- Seq1.fromSeq merge.commits
          syncStanza
            Sync
              { commits = commits1,
                success = null merge.conflicts,
                source = upstream,
                target = context.branch
              },
        do
          commits <- Seq1.fromSeq push.commits
          syncStanza
            Sync
              { commits,
                success = pushPushed push,
                source = context.branch,
                target = upstream
              },
        do
          conflicts1 <- List1.nonEmpty merge.conflicts <|> List1.nonEmpty stashConflicts
          conflictsStanza "These files are in conflict:" conflicts1,
        case push.what of
          NothingToPush2 -> Nothing
          Pushed -> Nothing
          PushWouldntReachRemote -> runSyncStanza "When you come online, run" context.branch upstream
          PushWouldBeRejected ->
            Just $
              "  Resolve the conflicts, then run "
                <> Text.bold (Text.blue "mit commit")
                <> " to synchronize "
                <> branchb context.branch
                <> " with "
                <> branchb upstream
                <> "."
          TriedToPush -> runSyncStanza "Run" context.branch upstream,
        if not (null state.undos) then canUndoStanza else Nothing
      ]

-- FIXME output what we just undid
mitUndo :: Mit Env [Stanza] ()
mitUndo = do
  context <- getContext
  case List1.nonEmpty context.state.undos of
    Nothing -> throw [Just (Text.red "Nothing to undo.")]
    Just undos1 -> for_ undos1 applyUndo
  when (undosContainRevert context.state.undos) mitSync
  where
    undosContainRevert :: [Undo] -> Bool
    undosContainRevert = \case
      [] -> False
      Revert _ : _ -> True
      _ : undos -> undosContainRevert undos

-- FIXME this type kinda sux now, replace with GitMerge probably?
data Sync = Sync
  { commits :: Seq1 GitCommitInfo,
    success :: Bool,
    source :: Text,
    target :: Text
  }

canUndoStanza :: Stanza
canUndoStanza =
  Just ("  Run " <> Text.bold (Text.blue "mit undo") <> " to undo this change.")

conflictsStanza :: Text.Builder -> List1 GitConflict -> Stanza
conflictsStanza prefix conflicts =
  Just $
    "  "
      <> prefix
      <> Builder.newline
      <> Builder.vcat ((\conflict -> "    " <> Text.red (showGitConflict conflict)) <$> conflicts)

isSynchronizedStanza2 :: Text -> GitPushWhat -> Stanza
isSynchronizedStanza2 branch = \case
  NothingToPush2 -> synchronizedStanza branch upstream
  Pushed -> synchronizedStanza branch upstream
  PushWouldntReachRemote -> notSynchronizedStanza branch upstream " because you appear to be offline."
  PushWouldBeRejected -> notSynchronizedStanza branch upstream "; their histories have diverged."
  TriedToPush -> notSynchronizedStanza branch upstream (" because " <> Text.bold "git push" <> " failed.")
  where
    upstream = "origin/" <> branch

notSynchronizedStanza :: Text -> Text -> Text.Builder -> Stanza
notSynchronizedStanza branch other suffix =
  Just ("  " <> Text.red (branchb branch <> " is not synchronized with " <> branchb other <> suffix))

runSyncStanza :: Text.Builder -> Text -> Text -> Stanza
runSyncStanza prefix branch upstream =
  Just $
    "  "
      <> prefix
      <> " "
      <> Text.bold (Text.blue "mit sync")
      <> " to synchronize "
      <> branchb branch
      <> " with "
      <> branchb upstream
      <> "."

syncStanza :: Sync -> Stanza
syncStanza sync =
  Just $
    Text.italic
      (colorize ("    " <> Text.Builder.fromText sync.source <> " → " <> Text.Builder.fromText sync.target))
      <> "\n"
      <> (Builder.vcat ((\commit -> "    " <> prettyGitCommitInfo commit) <$> commits'))
      <> (if more then "    ..." else Builder.empty)
  where
    colorize :: Text.Builder -> Text.Builder
    colorize =
      if sync.success then Text.green else Text.red
    (commits', more) =
      case Seq1.length sync.commits > 10 of
        False -> (Seq1.toSeq sync.commits, False)
        True -> (Seq1.dropEnd 1 sync.commits, True)

synchronizedStanza :: Text -> Text -> Stanza
synchronizedStanza branch other =
  Just ("  " <> Text.green (branchb branch <> " is synchronized with " <> branchb other <> "."))

-- FIXME remove
whatNextStanza :: Text -> PushResult -> Stanza
whatNextStanza branch = \case
  PushAttempted False -> runSyncStanza "Run" branch upstream
  PushAttempted True -> Nothing
  PushNotAttempted (ForkedHistory conflicts) ->
    Just $
      if null conflicts
        then
          "  Run "
            <> sync
            <> ", examine the repository, then run "
            <> sync
            <> " again to synchronize "
            <> branchb branch
            <> " with "
            <> branchb upstream
            <> "."
        else
          "  Run "
            <> sync
            <> ", resolve the conflicts, then run "
            <> commit
            <> " to synchronize "
            <> branchb branch
            <> " with "
            <> branchb upstream
            <> "."
  PushNotAttempted MergeConflicts ->
    Just ("  Resolve the merge conflicts, then run " <> commit <> ".")
  PushNotAttempted NothingToPush -> Nothing
  PushNotAttempted Offline -> runSyncStanza "When you come online, run" branch upstream
  PushNotAttempted UnseenCommits -> runSyncStanza "Examine the repository, then run" branch upstream
  where
    commit = Text.bold (Text.blue "mit commit")
    sync = Text.bold (Text.blue "mit sync")
    upstream = "origin/" <> branch

branchb :: Text -> Text.Builder
branchb =
  Text.italic . Text.Builder.fromText

------------------------------------------------------------------------------------------------------------------------
-- Context

data Context = Context
  { branch :: Text,
    snapshot :: GitSnapshot,
    state :: MitState (),
    upstreamHead :: Maybe Text
  }

getContext :: Mit Env x Context
getContext = do
  gitFetch_ "origin"
  branch <- Git.git Git.BranchShowCurrent
  upstreamHead <- gitRemoteBranchHead "origin" branch
  state <- readMitState branch
  snapshot <- performSnapshot
  pure Context {branch, snapshot, state, upstreamHead}

contextExistLocalCommits :: Context -> Mit Env x Bool
contextExistLocalCommits context =
  case context.upstreamHead of
    Nothing -> pure True
    Just upstreamHead -> gitExistCommitsBetween upstreamHead context.snapshot.head

contextExistRemoteCommits :: Context -> Mit Env x Bool
contextExistRemoteCommits context =
  case context.upstreamHead of
    Nothing -> pure False
    Just upstreamHead -> gitExistCommitsBetween context.snapshot.head upstreamHead

------------------------------------------------------------------------------------------------------------------------
-- Git merge

-- | The result of a @git merge@.
--
-- Impossible case: Impossible case: empty list of commits, non-empty list of conflicts.
data GitMerge = GitMerge
  { -- | The list of commits that were applied (or would be applied once conflicts are resolved), minus the merge commit
    -- itself.
    commits :: Seq GitCommitInfo,
    conflicts :: [GitConflict]
  }

-- Perform a fast-forward-if-possible git merge, and return the commits that were applied (or *would be* applied) (minus
-- the merge commit), along with the conflicting files. Impossible case: empty list of commits, non-empty list of
-- conflicts.
--
-- Precondition: the working directory is clean. TODO take unused GitStash as argument?
performMerge :: Text -> Text -> Mit Env x GitMerge
performMerge message commitish = do
  head <- gitHead
  commits <- gitCommitsBetween (Just head) commitish
  conflicts <-
    if Seq.null commits
      then pure []
      else
        git ["merge", "--ff", "--no-commit", commitish] >>= \case
          False -> gitConflicts
          True -> do
            -- If this was a fast-forward, a merge would not be in progress at this point.
            whenM gitMergeInProgress (git_ ["commit", "--message", message])
            pure []
  pure GitMerge {commits, conflicts}

------------------------------------------------------------------------------------------------------------------------
-- Git push

data GitPush = GitPush
  { commits :: Seq GitCommitInfo,
    undo :: [Undo],
    what :: GitPushWhat
  }

data GitPushWhat
  = NothingToPush2
  | Pushed
  | PushWouldntReachRemote
  | PushWouldBeRejected
  | TriedToPush

pushPushed :: GitPush -> Bool
pushPushed push =
  case push.what of
    NothingToPush2 -> False
    Pushed -> True
    PushWouldntReachRemote -> False
    PushWouldBeRejected -> False
    TriedToPush -> False

-- TODO get context
performPush :: Text -> Mit Env x GitPush
performPush branch = do
  fetched <- gitFetch "origin"
  head <- gitHead
  upstreamHead <- gitRemoteBranchHead "origin" branch
  commits <- gitCommitsBetween upstreamHead head

  if Seq.null commits
    then pure GitPush {commits, undo = [], what = NothingToPush2}
    else do
      existRemoteCommits <- maybe (pure False) (gitExistCommitsBetween head) upstreamHead
      if existRemoteCommits
        then pure GitPush {commits, undo = [], what = PushWouldBeRejected}
        else
          if fetched
            then
              gitPush branch >>= \case
                False -> pure GitPush {commits, undo = [], what = TriedToPush}
                True -> do
                  undo <-
                    if Seq.length commits == 1
                      then
                        gitIsMergeCommit head <&> \case
                          False -> [Revert head]
                          True -> []
                      else pure []
                  pure GitPush {commits, undo, what = Pushed}
            else pure GitPush {commits, undo = [], what = PushWouldntReachRemote}

------------------------------------------------------------------------------------------------------------------------
-- Git snapshot

data GitSnapshot = GitSnapshot
  { head :: Text,
    stash :: Maybe Text
  }

performSnapshot :: Mit Env x GitSnapshot
performSnapshot = do
  head <- gitHead
  stash <-
    gitDiff >>= \case
      Differences -> Just <$> gitCreateStash
      NoDifferences -> pure Nothing
  pure GitSnapshot {head, stash}

undoToSnapshot :: GitSnapshot -> [Undo]
undoToSnapshot snapshot =
  Reset snapshot.head : case snapshot.stash of
    Nothing -> []
    Just stash -> [Apply stash]
