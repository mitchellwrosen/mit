# mit

`mit` is a wrapper around `git` that aims to improve its UX by exposing high-level commands, synchronizing with the
remote repository automatically, providing informative and visual feedback, and allowing the user to undo the last
modification whenever possible.

As compared to `git`, which encourages siloed, offline development until an acceptable level of quality and completeness
is reached and commit history rewritten to show a pristine path, `mit` follows the general philosophy that development
should be iterative and highly collaborative, checkins should occur frequently, history should be preserved accurately,
and it is perfectly okay to have points in the history that do not even build, let alone work correctly.

## The full `mit commit` algorithm

`mit commit`, the flagship command that `mit` exposes, is semantically equivalent to something like
`git pull && git commit && git push` - that is, whenever recording a commit locally, first synchronize with the remote
repository, then record the commit, then publish it to the remote repository. However, there are a number of different
edge cases that make a simple `git` alias impractical.

Below is a detailed description of the `mit commit` algorithm. Assuming the user is on a branch called `feature`,

  - `mit` attempts to `git fetch origin` to ascertain where `origin/feature` is relative to `feature`. (`mit` assumes
    that every branch has an equivalently named tracking branch on the remote called `origin`).
  - If the fetch fails, `mit commit` proceeds in "offline mode", which will ultimately record a local commit that will
    not be published yet.
  - `mit` checks to see if the working tree is dirty, _including_ previously untracked files, because `mit` assumes that
    files that should not be tracked will be `.gitignore`d. (Note: there is currently an unfortunate dependency on `git`
    version `2.30.1` or greater, due to a bug in how `git stash` used to interact with untracked files that marked with
    `git add --intent-to-add`. In the future, `mit` may adjust its algorithm to work around this bug, but for now, you
    simply cannot use `mit commit` whenever tracking a new file, you must instead use `git` manually).
  - If the working tree is not dirty, there is nothing to commit. In this case, `mit commit` falls back to `mit sync`,
    described below.
  - Otherwise, if the working tree is dirty, `mit` synchronizes as best as it can with `origin/feature`.
    - If `origin/feature` is not ahead of `feature`, there is nothing left to do.
    - If `origin/feature` is ahead of `feature`, `mit` first stashes the uncommitted changes, then attempts to merge in
      `origin/feature` (preferring a fast-forward merge, if possible).
      - If the merge fails with conflicts, `mit` aborts the merge, because `feature` has already diverged from
        `origin/feature`, prior to attempting to record this commit. `mit` resets to the state prior to attempting the
        merge.
      - If the merge succeeds, `mit` attempts to apply the stash.
        - If applying the stash fails, `mit` again resets to the state prior to attempting the merge.
        - If applying the stash succeeds, there is nothing left to do.
  - After synchronizing, the uncommitted changes are as they were prior to running `mit commit`, but the working tree
    may have advanced to `origin/feature`, if it was possible to get there via either a fast-forward or traditional
    merge without conflicts. This means that the commit may be recorded atop a state of the repository that was unknown
    to the committer prior to running `mit commit`, and a follow-up commit may be necessary to address any defects that
    were introduced due to the merge. Nonetheless, this behavior is intentional, as `mit` encourages iterative,
    collaborative development, which often involves making minor fix-ups due to recently introduced incompatibilities.
  - `mit` prepares a commit interactively with `git commit --patch`. (Note that this commit may be aborted by staging no
    changes, or providing an empty commit message).
  - Whether or not the commit was aborted, if any conflicts were observed while synchronizing with `origin/feature`,
    `mit` performs a follow-up merge, which will fail.
  - `mit` _commits the conflicting merge bubble as-is_, noting which files are in conflict, to report later. This is
    done so a subsequent commit can clearly show how the conflicts were resolved, possibly after publishing the broken
    commit so the conflicts can be resolved collaboratively. `git`, by contrast, encourages the developer to resolve
    conflicts alone, and worse, does not provide a convenient mechanism for other developers to witness how exactly the
    conflicts were resolved.
  - `mit` attempts to push `feature` to `origin/feature` if there were no conflicts (because otherwise the local
    repository's latest commit is a merge bubble with conflict markers), the initial fetch did not fail (indicating the
    user is probably offline), and there is at least one commit reachable by `feature` but not `origin/feature` (either
    recorded previously, or just now - but recall that the current commit may have been aborted).
  - `mit` determines whether all of the above can be undone with a `mit undo`, and if so, records some undo metadata
    in a file in the `.git` directory.
  - `mit` provides a summary of what just occurred, indicating which previously unseen commits from `origin/feature`
    were applied to `feature`, which previously unpublished commits from `feature` were published to `origin/feature`,
    and which files currently have conflict markers in them (but are not in conflict according to `git`).

## The full `mit sync` algorithm

  - TODO
