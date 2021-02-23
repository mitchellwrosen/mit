# mit

Whenever working with other developers on a feature branch, `git`'s default behaviors can become tiresome. It is easy to
forget to push, or forget to pull, and end up causing unnecessary merge conflicts due to patches propagating too slowly
from one machine to another.

And even when working alone, `git`'s UX is notoriously poor, partially because it is so heavily used, and thus committed
to maintaining backwards compatibility with previously released versions, but partially because it was designed first
and foremost for the Linux kernel developer community, whose needs are unique, and distinct from most other software
projects, which are built and maintained by much smaller groups of people, all in close communication.

Essentially, `git`'s "porcelain" commands often feel low-level enough to be considered "plumbing". Enter Mitchell's
opinionated and cleverly-named `git` wrapper, which codifies some of Mitchell's strong but loosely-held opinions on how
`git` should be used in small- to medium-sized teams, since that's all he has experience with.

* The history of a feature branch should record how it was developed, not mutated and prettified or the fact. Commits to
  a feature branch should not be amended nor rebased.
* Commits to a feature branch do not individually need to pass any test suites or other CI checks. They are nothing more
  than individual units of work that are ready to be shared with other developers working on the same feature branch.
* Commits to a feature branch should be published as soon as possible, to reduce the likelihood of a fork in history.
* Forks in a feature branch's history should be resolved as soon as possible.
* When a feature branch is merged into a mainline development branch, its commits may be squashed, so the development
  branch remains bisectable.
* There is no need to use the git index. When you're ready to commit changes, just prepare the patch
  interactively.
* There is no need to switch branches in-place. If you want to work on different features in parallel, use worktrees
  instead.
* There is little need to use the git stash.
* Each repo should only have a single remote repository called "origin", and each local branch should track its
  corresponding branch on "origin".
* Untracked files that are not meant to be committed should just be `.gitignore`d away.

All that said, this is just a `git` wrapper that exposes high-level commands that are inspired by the philosophy above.
I encourage you to try running it with the `debug=` environment variable to see the actual commands that are invoked,
steal some ideas, and write your own tool.

#### mit clone ≪repo≫

Clone a repo separately from its `.git` directory, so that the project folder structure can accomodate multiple
worktrees. Running `mit clone git@github.com:mitchellwrosen/mit.git` will create the following folder structure:

    mit/
      .git/
      master/


#### mit commit

Commit and push the working tree.

In detail:

- Run `git reset` and `git add --all --intent-to-add` in order to "yellowize" all files (that is, unstage green
files and start tracking red ones).
- Run `git diff`.
  - If it reports there are no changes in the working tree, exit.
  - Otherwise,
    - Run `git fetch origin` in order to have up-to-date local references.
    - Run `git branch --show-current` to get the name of the current branch.
    - Run `git show-ref` to see if there is a reference called `refs/remotes/origin/≪branch≫`.
      - If there isn't, that means this branch has not yet been published, so
        - run `git commit --patch` then
        - run `git push --set-upstream origin`.
      - Otherwise, run `git rev-list` to determine if the upstream branch can reach any commits that the current branch
        cannot, which is assumed to mean it is strictly ahead, as opposed to totally detached (because someone
        force-pushed it to somewhere wacky).
        - If it is not ahead,
          - run `git commit --patch`, then
          - run `git push origin`.
        - Otherwise,
          - run `git stash push` to stash the changes we intend to commit.
          - Run `git merge --ff ≪branch≫` to merge in the upstream changes, possibly with a merge bubble (as we do not
            pass `--no-commit`), but preferring a fast-forward.
            - If the merge fails with conflicts, that means our local history has already forked from upstream, and our
              desire to lay one more commit on top doesn't change that. So,
              - run `git merge --abort` to abort the merge,
              - pop the stash with `git stash pop`, and
              - run `git commit --patch`.
              - Do not attempt to publish the commit (since it would fail) - just warn that there is a fork, and
              - recommend running `mit sync`.
            - Otherwise, if the merge succeeds, pop the stash with `git stash pop`.
              - If that fails with conflicts, it means that although nothing prior to the latest uncommitted changes
                conflicted with upstream, the latest uncommitted changes nonetheless did. However, we really do want to
                make a commit (that's why we typed `mit commit`), not resolve conflicts, so
                - run `git reset` to reset to the working tree prior to merging,
                - pop the stash with `git stash pop` (the prior pop, which resulted in conflicts, left the stash entry
                  to be popped again), and
                - run `git commit --patch`.
                - Again in this case, do not push, but merely warn that there is a fork, and
                - recommend running `mit sync`.
              - Otherwise, if popping the stash succeeds,
                - report the new commits that resulted from the merge.
                - Do not actually perform the final commit, because new commits have arrived underneath it, and it is
                  possible that, although everything merged cleanly, there is some new issue to address.
                - Recommend running `mit commit` again if everything looks good.

#### mit sync [≪branch≫]

"Synchronize" a branch with the current. This is essentially a synonym of "merge", except that, just like `git merge`, a
"merge bubble" will not necessarily be created, because fast-forward merges are preferred. So some other verb seems
appropriate.

A sort of implicit `mit sync` occurs whenever recording changes with `mit commit`.

In detail:

- Run `git fetch origin` in order to have up-to-date local references.
- If no explicit branch was provided to `mit sync`, run `git branch show-current` to default it to the current branch.
- Run `git show-ref` to see if there is a reference called `refs/remotes/origin/≪branch≫`.
  - If there is, use that branch as the merge target instead, as it is assumed to be more up-to-date than the local
    copy, if there even is one.
- Run `git reset` and `git add --all --intent-to-add` in order to "yellowize" all files (that is, unstage green files
  and start tracking red ones).
- Run `git diff`, and if it reports there are any changes in the working tree, stash them with `git stash create`.
- Run `git merge --ff ≪branch≫` to merge in the changes, possibly with a merge bubble (as we do not pass `--no-commit`),
  but preferring a fast-forward).
  - If the merge fails with conflicts,
    - if we have stashed uncommitted changes, use `git merge --abort` to reset the working tree to the state it was in
      before `mit sync` and exit. (This is only a temporary restriction).
    - Otherwise, if we have not stashed uncommitted changes, report the conflicting files, and recommend either fixing
      the conflicts and running `mit commit`, or running `mit abort`.
  - Otherwise, if the merge succeeds,
    - if we have stashed uncommitted changes, try applying them with `git stash apply`.
      - If that fails,
        - report the new commits that result from the merge, and explain that there are now conflicts due to the
          uncommitted changes.
        - Store some metadata that allows the user to elect to abandon the synchronize effort entirely with `mit abort`,
          or else contine working and eventually `mit commit` the resolved conflicts, and possibly more.
      - Otherwise, if applying the previously uncommitted changes succeeds, report the new commits that result from the
        merge.
    - Otherwise, if we have not stashed uncommitted changes, report the new commits that result from the merge.
