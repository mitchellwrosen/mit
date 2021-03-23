{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Mit where

import qualified Data.List.NonEmpty as List1
import qualified Data.Text as Text
import qualified Data.Text.ANSI as Text
import qualified Data.Text.Encoding.Base64 as Text
import qualified Data.Text.IO as Text
import Mit.Git
import Mit.Prelude
import qualified System.Clock as Clock
import System.Directory (doesDirectoryExist, removeFile, withCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)

-- FIXME: nicer "git status" story. in particular the conflict markers in the commits after a merge are a bit
-- ephemeral feeling
-- FIXME bail if active cherry-pick, active revert, active rebase, what else?
-- FIXME rev-list max 11, use ellipses after 10
-- FIXME test file deleted by us/them conflict

-- TODO mit init
-- TODO mit delete-branch
-- TODO tweak things to work with git < 2.30.1
-- TODO rewrite mit commit algorithm in readme
-- TODO git(hub,lab) flow or something?
-- TODO 'mit branch' with dirty working directory - apply changes to new worktree?
-- TODO undo in more cases?
-- TODO recommend merging master if it conflicts
-- TODO mit log
-- TODO optparse-applicative

main :: IO ()
main = do
  getArgs >>= \case
    ["branch", branch] -> mitBranch (Text.pack branch)
    ["clone", parseGitRepo . Text.pack -> Just (url, name)] -> mitClone url name
    ["commit"] -> mitCommit
    ["merge", branch] -> mitMerge (Text.pack branch)
    ["sync"] -> mitSync
    ["undo"] -> mitUndo
    _ -> do
      putLines
        [ "Usage:",
          "  mit branch ≪branch≫",
          "  mit clone ≪repo≫",
          "  mit commit",
          "  mit merge ≪branch≫",
          "  mit sync",
          "  mit undo"
        ]
      exitFailure

dieIfBuggyGit :: IO ()
dieIfBuggyGit = do
  version <- gitVersion
  case foldr (\(ver, err) acc -> if version < ver then (ver, err) : acc else acc) [] validations of
    [] -> pure ()
    errors ->
      die $
        map
          (\(ver, err) -> "Prior to " <> Text.bold "git" <> " version " <> showGitVersion ver <> ", " <> err)
          errors
  where
    validations :: [(GitVersion, Text)]
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

dieIfMergeInProgress :: IO ()
dieIfMergeInProgress =
  whenM gitMergeInProgress (die [Text.bold "git merge" <> " in progress."])

dieIfNotInGitDir :: IO ()
dieIfNotInGitDir =
  try (evaluate gitdir) >>= \case
    Left (_ :: ExitCode) -> exitFailure
    Right _ -> pure ()

die :: [Text] -> IO a
die ss = do
  Text.putStr (Text.red (Text.unlines ss))
  exitFailure

mitBranch :: Text -> IO ()
mitBranch branch = do
  dieIfNotInGitDir

  gitBranchWorktreeDir branch >>= \case
    Nothing ->
      doesDirectoryExist (Text.unpack worktreeDir) >>= \case
        False -> do
          git_ ["worktree", "add", "--detach", worktreeDir]
          withCurrentDirectory (Text.unpack worktreeDir) do
            gitBranchExists branch >>= \case
              False -> do
                gitBranch branch
                gitSwitch branch
                gitFetch_ "origin"
                whenM (gitRemoteBranchExists "origin" branch) do
                  git_ ["reset", "--hard", upstream]
                  git_ ["branch", "--set-upstream-to", upstream]
              True -> gitSwitch branch
        True -> die ["Directory " <> Text.bold worktreeDir <> " already exists."]
    Just directory ->
      unless (directory == worktreeDir) do
        die [Text.bold branch <> " is already checked out in " <> Text.bold directory <> "."]
  where
    upstream :: Text
    upstream =
      "origin/" <> branch

    worktreeDir :: Text
    worktreeDir =
      Text.dropWhileEnd (/= '/') gitdir <> branch

mitClone :: Text -> Text -> IO ()
mitClone url name =
  -- FIXME use 'git config --get init.defaultBranch'
  git ["clone", url, "--separate-git-dir", name <> "/.git", name <> "/master"]

mitCommit :: IO ()
mitCommit = do
  dieIfNotInGitDir
  whenM gitExistUntrackedFiles dieIfBuggyGit
  gitDiff >>= \case
    Differences -> pure ()
    NoDifferences -> exitFailure
  context <- makeContext
  gitMergeInProgress >>= \case
    False -> mitCommitWith context
    True -> mitCommitMerge context

mitCommitMerge :: Context -> IO ()
mitCommitMerge context = do
  maybeState0 <- readMitState context.branch64
  let maybeMerging = do
        state0 <- maybeState0
        state0.merging
  case maybeMerging of
    Nothing -> git_ ["commit", "--all", "--no-edit"]
    Just merging ->
      let message =
            fold
              [ "⅄ ",
                if merging == context.branch then "" else (merging <> " → "),
                context.branch
              ]
       in git_ ["commit", "--all", "--message", message]
  let undos = maybe [] (.undos) maybeState0
  case [commit | Apply commit <- undos] of
    [] -> mitSyncWith context (Just [Reset context.head])
    stash : _ ->
      gitApplyStash stash >>= \case
        -- FIXME we just unstashed, now we're about to stash again :/
        [] -> mitSyncWith context (Just [Reset context.head, Apply stash])
        stashConflicts -> do
          head <- git ["rev-parse", "HEAD"]
          writeMitState context MitState {head, merging = Nothing, ranCommitAt = Nothing, undos}
          putSummary
            Summary
              { branch = context.branch,
                canUndo = not (null undos),
                conflicts = stashConflicts,
                syncs = []
              }

mitCommitWith :: Context -> IO ()
mitCommitWith context = do
  maybeUpstreamHead :: Maybe Text <-
    git ["rev-parse", "refs/remotes/" <> context.upstream] <&> \case
      Left _ -> Nothing
      Right upstreamHead -> Just upstreamHead

  remoteCommits :: [GitCommitInfo] <-
    case maybeUpstreamHead of
      Nothing -> pure []
      Just upstreamHead -> gitCommitsBetween (Just context.head) upstreamHead

  localCommits0 <- gitCommitsBetween maybeUpstreamHead "HEAD"
  maybeState0 <- readMitState context.branch64

  shouldWarnAboutFork <- do
    let wouldFork = not (null remoteCommits) && null localCommits0
    case wouldFork of
      False -> pure False
      True -> do
        let theyRanMitCommitRecently =
              case maybeState0 of
                Just MitState {ranCommitAt = Just t0} -> do
                  t1 <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
                  pure ((t1 - t0) < 10_000_000_000)
                _ -> pure False
        not <$> theyRanMitCommitRecently

  case shouldWarnAboutFork of
    False -> do
      stash <- gitCreateStash
      commitResult <- gitCommit
      localCommits <- if commitResult then gitCommitsBetween maybeUpstreamHead "HEAD" else pure localCommits0

      pushResult <-
        if
            | null localCommits -> pure (PushNotAttempted PushNoCommits)
            | not (null remoteCommits) -> pure (PushNotAttempted PushWouldConflict)
            | context.fetchFailed -> pure (PushNotAttempted PushOffline)
            | otherwise -> PushAttempted <$> gitPush context.branch

      state1 <- do
        ranCommitAt <-
          case commitResult of
            False -> Just . Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
            True -> pure Nothing
        undos <-
          let onDidntPush =
                case commitResult of
                  False -> maybe [] (.undos) maybeState0
                  True -> [Reset context.head, Apply stash]
           in case pushResult of
                PushAttempted True ->
                  case (commitResult, localCommits) of
                    (True, [_]) -> do
                      head <- git ["rev-parse", "HEAD"]
                      pure [Revert head, Apply stash]
                    _ -> pure []
                PushAttempted False -> pure onDidntPush
                PushNotAttempted _ -> pure onDidntPush
        pure MitState {head = (), merging = Nothing, ranCommitAt, undos}

      writeMitState context state1

      putSummary
        Summary
          { branch = context.branch,
            -- Whether we "can undo" from here is not exactly if the state says we can undo, because of one corner case:
            -- we ran 'mit commit', then aborted the commit, and ultimately didn't push any other local changes.
            --
            -- In this case, the underlying state hasn't changed, so 'mit undo' will still work as if the 'mit commit'
            -- was never run, we merely don't want to *say* "run 'mit undo' to undo" as feedback, because that sounds as
            -- if it would undo the last command run, namely the 'mit commit' that was aborted.
            canUndo = not (null state1.undos) && commitResult,
            conflicts = [],
            syncs =
              [ Sync
                  { commits = localCommits,
                    result = pushResultToSyncResult pushResult,
                    source = context.branch,
                    target = context.upstream
                  }
              ]
          }
    True -> do
      ranCommitAt <- Just . Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
      let state0 =
            case maybeState0 of
              Nothing -> MitState {head = (), merging = Nothing, ranCommitAt, undos = []}
              Just s0 -> s0 {ranCommitAt}
      writeMitState context state0
      putLines
        [ "",
          "  " <> Text.italic context.branch <> " is not up to date.",
          "",
          "  Run " <> Text.bold (Text.blue "mit sync") <> " first, or run " <> Text.bold (Text.blue "mit commit")
            <> " again to record a commit anyway.",
          ""
        ]
      exitFailure

data PushResult
  = PushAttempted Bool
  | PushNotAttempted PushNotAttemptedReason

data PushNotAttemptedReason
  = PushCommitHasConflicts -- local commit has conflict markers
  | PushNewCommits -- we just pulled remote commits; don't push in case there's something local to address
  | PushNoCommits -- no commits to push
  | PushOffline -- fetch failed, so we seem offline
  | PushWouldConflict -- local history has forked, need to sync

pushResultToSyncResult :: PushResult -> SyncResult
pushResultToSyncResult = \case
  PushAttempted True -> Success
  PushAttempted False -> Failure
  PushNotAttempted PushCommitHasConflicts -> Failure
  PushNotAttempted PushNewCommits -> Pending
  PushNotAttempted PushNoCommits -> Success -- doesnt matter, wont be shown
  PushNotAttempted PushOffline -> Offline
  PushNotAttempted PushWouldConflict -> Failure

-- FIXME if on branch 'foo', handle 'mitMerge foo' or 'mitMerge origin/foo' as 'mitSync'?
mitMerge :: Text -> IO ()
mitMerge target = do
  dieIfNotInGitDir
  dieIfMergeInProgress
  whenM gitExistUntrackedFiles dieIfBuggyGit

  context <- makeContext

  -- When given 'mit merge foo', prefer merging 'origin/foo' over 'foo'
  targetCommit <- do
    git ["rev-parse", "origin/" <> target] >>= \case
      Left _ ->
        git ["rev-parse", target] >>= \case
          Left _ -> exitFailure
          Right commit -> pure commit
      Right commit -> pure commit

  mergeStatus <- mitMerge' ("⅄ " <> target <> " → " <> context.branch) targetCommit

  writeMitState
    context
    MitState
      { head = (),
        merging =
          case mergeStatus of
            MergeStatus'Merge _ (MergeResult'MergeConflicts _) _ -> Just target
            MergeStatus'Merge _ (MergeResult'StashConflicts _) _ -> Nothing
            MergeStatus'NoMerge -> Nothing,
        ranCommitAt = Nothing,
        undos =
          case mergeStatus of
            MergeStatus'Merge _ _ undos -> undos
            MergeStatus'NoMerge -> []
      }

  putSummary
    Summary
      { branch = context.branch,
        canUndo =
          case mergeStatus of
            MergeStatus'Merge _ _ _ -> True
            MergeStatus'NoMerge -> False,
        -- At most one of stash/merge conflicts is non-null, so just appending them makes some sense
        conflicts =
          case mergeStatus of
            MergeStatus'Merge _commits result _undos ->
              case result of
                MergeResult'MergeConflicts conflicts -> List1.toList conflicts
                MergeResult'StashConflicts conflicts -> conflicts
            MergeStatus'NoMerge -> [],
        syncs =
          case mergeStatus of
            MergeStatus'NoMerge -> []
            MergeStatus'Merge commits result _undos ->
              [ Sync
                  { commits = List1.toList commits,
                    result =
                      case result of
                        MergeResult'MergeConflicts _ -> Failure
                        -- Even if we have conflicts from unstashing, we call this merge a success.
                        MergeResult'StashConflicts _ -> Success,
                    source = target,
                    target = context.branch
                  }
              ]
      }

data MergeStatus
  = MergeStatus'NoMerge
  | MergeStatus'Merge (List1 GitCommitInfo) MergeResult [Undo]

data MergeResult
  = MergeResult'MergeConflicts (List1 GitConflict)
  | MergeResult'StashConflicts [GitConflict]

mitMerge' :: Text -> Text -> IO MergeStatus
mitMerge' message target = do
  head <- git ["rev-parse", "HEAD"]
  (List1.nonEmpty <$> gitCommitsBetween (Just head) target) >>= \case
    Nothing -> pure MergeStatus'NoMerge
    Just commits -> do
      maybeStash <- gitStash
      let undos = Reset head : maybeToList (Apply <$> maybeStash)
      result <-
        git ["merge", "--ff", "--no-commit", target] >>= \case
          False -> do
            conflicts <- gitConflicts
            pure (MergeResult'MergeConflicts (List1.fromList conflicts))
          True -> do
            whenM gitMergeInProgress (git_ ["commit", "--message", message])
            maybeConflicts <- for maybeStash gitApplyStash
            pure (MergeResult'StashConflicts (fromMaybe [] maybeConflicts))
      pure (MergeStatus'Merge commits result undos)

-- TODO implement "lateral sync", i.e. a merge from some local or remote branch, followed by a sync to upstream
mitSync :: IO ()
mitSync = do
  dieIfNotInGitDir
  dieIfMergeInProgress
  whenM gitExistUntrackedFiles dieIfBuggyGit
  context <- makeContext
  mitSyncWith context Nothing

-- | @mitSyncWith context maybeUndos@
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
mitSyncWith :: Context -> Maybe [Undo] -> IO ()
mitSyncWith context maybeUndos = do
  maybeUpstreamHead :: Maybe Text <-
    git ["rev-parse", "refs/remotes/" <> context.upstream] <&> \case
      Left _ -> Nothing
      Right upstreamHead -> Just upstreamHead

  mergeStatus <-
    case maybeUpstreamHead of
      Nothing -> pure MergeStatus'NoMerge
      Just upstreamHead -> mitMerge' ("⅄ " <> context.branch) upstreamHead

  localCommits :: [GitCommitInfo] <-
    gitCommitsBetween maybeUpstreamHead "HEAD"

  pushResult :: PushResult <-
    case localCommits of
      [] -> pure (PushNotAttempted PushNoCommits)
      _ ->
        case mergeStatus of
          MergeStatus'NoMerge ->
            case context.fetchFailed of
              False -> PushAttempted <$> gitPush context.branch
              True -> pure (PushNotAttempted PushOffline)
          MergeStatus'Merge _commits result _undos ->
            case result of
              MergeResult'MergeConflicts _conflicts -> pure (PushNotAttempted PushWouldConflict) -- FIXME better reason
              MergeResult'StashConflicts _conflicts -> pure (PushNotAttempted PushNewCommits)

  let undos =
        let undoMerge =
              case mergeStatus of
                MergeStatus'Merge _commits _result us -> us
                MergeStatus'NoMerge -> []
         in case pushResult of
              PushAttempted True -> []
              PushAttempted False -> fromMaybe undoMerge maybeUndos
              PushNotAttempted _ -> fromMaybe undoMerge maybeUndos

  writeMitState
    context
    MitState
      { head = (),
        merging =
          case mergeStatus of
            MergeStatus'Merge _ (MergeResult'MergeConflicts _) _ -> Just context.branch
            MergeStatus'Merge _ (MergeResult'StashConflicts _) _ -> Nothing
            MergeStatus'NoMerge -> Nothing,
        ranCommitAt = Nothing,
        undos
      }

  putSummary
    Summary
      { branch = context.branch,
        canUndo = not (null undos),
        conflicts =
          case mergeStatus of
            MergeStatus'Merge _commits result _undos ->
              case result of
                MergeResult'MergeConflicts conflicts -> List1.toList conflicts
                MergeResult'StashConflicts conflicts -> conflicts
            MergeStatus'NoMerge -> [],
        syncs =
          [ Sync
              { commits =
                  case mergeStatus of
                    MergeStatus'Merge commits _result _undos -> List1.toList commits
                    MergeStatus'NoMerge -> [],
                result =
                  case mergeStatus of
                    MergeStatus'Merge _commits result _undos ->
                      case result of
                        MergeResult'MergeConflicts _ -> Failure
                        MergeResult'StashConflicts _ -> Success
                    MergeStatus'NoMerge -> Success,
                source = context.upstream,
                target = context.branch
              },
            Sync
              { commits = localCommits,
                result = pushResultToSyncResult pushResult,
                source = context.branch,
                target = context.upstream
              }
          ]
      }

-- FIXME output what we just undid
mitUndo :: IO ()
mitUndo = do
  dieIfNotInGitDir

  branch64 <- Text.encodeBase64 <$> gitCurrentBranch

  readMitState branch64 >>= \case
    Nothing -> exitFailure
    Just MitState {undos} -> do
      case List1.nonEmpty undos of
        Nothing -> exitFailure
        Just undos1 -> applyUndos undos1
      when (undosContainRevert undos) mitSync
  where
    undosContainRevert :: [Undo] -> Bool
    undosContainRevert = \case
      [] -> False
      Revert _ : _ -> True
      _ : undos -> undosContainRevert undos

data Context = Context
  { branch :: Text,
    branch64 :: Text,
    fetchFailed :: Bool,
    head :: Text,
    upstream :: Text
  }

makeContext :: IO Context
makeContext = do
  branch <- gitCurrentBranch
  head <- git ["rev-parse", "HEAD"]
  fetchFailed <- not <$> git ["fetch", "origin"]
  pure
    Context
      { branch,
        branch64 = Text.encodeBase64 branch,
        fetchFailed,
        head,
        upstream = "origin/" <> branch
      }

data Summary = Summary
  { branch :: Text,
    canUndo :: Bool,
    conflicts :: [GitConflict],
    syncs :: [Sync]
  }

data Sync = Sync
  { commits :: [GitCommitInfo],
    result :: SyncResult,
    source :: Text,
    target :: Text
  }

data SyncResult
  = Offline
  | Failure
  | Pending
  | Success

-- FIXME show some graph of where local/remote is at
putSummary :: Summary -> IO ()
putSummary summary =
  let output = concatMap syncLines summary.syncs ++ conflictsLines ++ undoLines
   in if null output then pure () else putLines ("" : output)
  where
    conflictsLines :: [Text]
    conflictsLines =
      if null summary.conflicts
        then []
        else
          "  The following files have conflicts." :
          map (("    " <>) . Text.red . showGitConflict) summary.conflicts ++ [""]
    syncLines :: Sync -> [Text]
    syncLines sync =
      if null sync.commits
        then []
        else
          colorize (Text.italic ("  " <> sync.source <> " → " <> sync.target)) :
          map (("  " <>) . prettyGitCommitInfo) sync.commits
            ++ [""]
      where
        colorize :: Text -> Text
        colorize =
          case sync.result of
            Offline -> Text.brightBlack
            Failure -> Text.red
            Pending -> Text.yellow
            Success -> Text.green
    undoLines :: [Text]
    undoLines =
      if summary.canUndo
        then ["  Run " <> Text.bold (Text.blue "mit undo") <> " to undo this change.", ""]
        else []

-- FIXME consolidate these files

-- State file

data MitState a = MitState
  { head :: a,
    merging :: Maybe Text,
    ranCommitAt :: Maybe Integer,
    undos :: [Undo]
  }
  deriving stock (Eq, Show)

deleteMitState :: Text -> IO ()
deleteMitState branch64 =
  removeFile (mitfile branch64) `catch` \(_ :: IOException) -> pure ()

parseMitState :: Text -> Maybe (MitState Text)
parseMitState contents = do
  [headLine, mergingLine, ranCommitAtLine, undosLine] <- Just (Text.lines contents)
  ["head", head] <- Just (Text.words headLine)
  merging <-
    case Text.words mergingLine of
      ["merging"] -> Just Nothing
      ["merging", branch] -> Just (Just branch)
      _ -> Nothing
  ranCommitAt <-
    case Text.words ranCommitAtLine of
      ["ran-commit-at"] -> Just Nothing
      ["ran-commit-at", text2int -> Just n] -> Just (Just n)
      _ -> Nothing
  undos <- Text.stripPrefix "undos " undosLine >>= parseUndos
  pure MitState {head, merging, ranCommitAt, undos}

readMitState :: Text -> IO (Maybe (MitState ()))
readMitState branch64 = do
  head <- git ["rev-parse", "HEAD"]
  try (Text.readFile (mitfile branch64)) >>= \case
    Left (_ :: IOException) -> pure Nothing
    Right contents -> do
      let maybeState = do
            state <- parseMitState contents
            guard (head == state.head)
            pure state
      case maybeState of
        Nothing -> do
          deleteMitState branch64
          pure Nothing
        Just state -> pure (Just (state {head = ()} :: MitState ()))

writeMitState :: Context -> MitState () -> IO ()
writeMitState context state = do
  head <- git ["rev-parse", "HEAD"]
  let contents :: Text
      contents =
        Text.unlines
          [ "head " <> head,
            "merging " <> fromMaybe Text.empty state.merging,
            "ran-commit-at " <> maybe Text.empty int2text state.ranCommitAt,
            "undos " <> showUndos state.undos
          ]
  Text.writeFile (mitfile context.branch64) contents `catch` \(_ :: IOException) -> pure ()

mitfile :: Text -> FilePath
mitfile branch64 =
  Text.unpack (gitdir <> "/.mit-" <> branch64)

-- Undo file utils

data Undo
  = Apply Text -- apply stash
  | Reset Text -- reset to commit
  | Revert Text -- revert commit
  deriving stock (Eq, Show)

showUndos :: [Undo] -> Text
showUndos =
  Text.intercalate " " . map showUndo
  where
    showUndo :: Undo -> Text
    showUndo = \case
      Apply commit -> "apply/" <> commit
      Reset commit -> "reset/" <> commit
      Revert commit -> "revert/" <> commit

parseUndos :: Text -> Maybe [Undo]
parseUndos t0 = do
  (Text.words >>> traverse parseUndo) t0
  where
    parseUndo :: Text -> Maybe Undo
    parseUndo text =
      asum
        [ Apply <$> Text.stripPrefix "apply/" text,
          Reset <$> Text.stripPrefix "reset/" text,
          Revert <$> Text.stripPrefix "revert/" text,
          error (show text)
        ]

applyUndos :: List1 Undo -> IO ()
applyUndos =
  traverse_ \case
    Apply commit -> do
      -- This should never return conflicts
      -- FIXME assert?
      _conflicts <- gitApplyStash commit
      pure ()
    Reset commit -> gitResetHard commit
    Revert commit -> git_ ["revert", commit]
