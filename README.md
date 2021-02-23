# mit

Let's see, where to even begin? Whenever working with other developers on a feature branch, `git`'s default behaviors
can become tiresome. It is easy to forget to push a commit, or forget to pull in the morning, and end up eventually
causing unnecessary delay or even merge conflicts due to patches generally propagating too slowly from one developer to
another.

And even when working alone, `git`'s UX is notoriously poor, partially because it is so heavily used, and thus committed
to maintaining backwards compatibility, but partially because it was designed first and foremost for the Linux kernel
developer community, whose needs are unique, and distinct from most other software projects, which are built and
maintained by much smaller groups of people in close communication.

Additionally, certain workflows and features are unintuitive, or feel discouraged. Worktrees, for example, are a
fantastic alternative to switching branches in-place, but they remain obscured by a clunky API and strange relationship
to other `git` abstractions, like branches.

Essentially, `git`'s "porcelain" commands often feel low-level enough to be considered "plumbing", and using `git`
directly at the command-line often feels like writing assembly code. Certainly in 2021, and probably for the next decade
or more, it is a tool well-worth mastering, despite its flaws. Yet, all users could benefit from a much higher-level and
streamlined interface. Enter Mitchell's opinionated and cleverly-named `git` wrapper, which codifies some of Mitchell's
strong but loosely-held beliefs about on how `git` should be used effectively, at least in in small- to medium-sized
teams. Beyond a certain throughput, all popular version control systems start to break down and require complicated,
supplemental server-side tooling.

In one sentence, this tool can be approximated as, "like `git`, but automatically pushes whenever you commit". To a
developer whose workflow is often a simple series of `git commit`s followed by a `git push`, and perhaps even an
occasional `git pull` whenever collaborating, this is indeed the most noticeable difference.

(Incidentally, implementing this correctly is what eventually led me to write this script. I think we can all imagine
how to write a `git` alias or two that can perform some sort of `git add && git commit && git push`, but what if
upstream is ahead? And if it is, what if the new commit conflicts? Or what if the local branch had *already* forked,
because we had previously made a couple commits while offline? Oh what if we're offline right now? We'll need some more
`git` aliases).

But the default commit behavior of the tools already suggest very different philosophies: while `git` encourages
developers to work in isolation, and hide their mistakes and missteps from each other until the history can be rewritten
to show only pristine activity, `mit` says: it's okay. Just do some work, snapshot it, and publish it. If you break the
build, you can fix it in the next commit. No one needs to run those tests right now, anyway.

Another example: whenever `mit` encounters a merge bubble with conflict markers, it makes a commit right away, and
reports which files are "in conflict" (but not according to `git`). This, too, is meant to record an accurate history of
what actually happened, and encourage collaboration by allowing a developer to prepare a patch that shows only how the
conflicts between one commit and another were resolved. With `git`'s default behavior of disallowing a merge to conclude
until all conflicts are marked as resolved, the developer, unless delinquent, is forced to work in isolation, and worse,
cannot easily communicate his or her work to others, as resolved merge conflicts are indistinguishable from all of the
innocuous noise that typically occupies a merge bubble.

But regardless of whatever development philosophy `mit` implicates, it should be pleasant to use. That means it should
handle edge-cases well, communicate relevant information at the right times, and not require anyone who isn't interested
in becoming a power user to read any sort of manual.

Below, in no particular order, is a set of rules and guidelines that, in one way or another, have influcened (or will
influence) some feature of `mit`.

* The history of a feature branch should record how it was developed, not mutated and prettified or the fact. Commits to
  a feature branch should not regularly need to be amended or rebased.
* Commits to a feature branch do not individually need to pass any test suites or other CI checks. They are nothing more
  than individual units of work that are ready to be shared with other developers working on the same feature branch.
* Merge commits with conflicts should always be immediately committed as such. Deleting conflict markers should always
  happen in a follow-on commit.
* Commits to a feature branch should be published as soon as possible, to reduce the likelihood of a fork in history.
* Forks in a feature branch's history should be resolved as soon as possible.
* There is no need to use the git index. When you're ready to commit changes, just prepare the patch interactively.
* There is no need to switch branches in-place. If you want to work on different features in parallel, use worktrees
  instead.
* There is little need to use the git stash.
* Untracked files that are not meant to be committed should just be `.gitignore`d away.
* When a feature branch is merged into a mainline development branch, its commits may be squashed, so the development
  branch remains bisectable.
* Each repository should only have a single remote repository called "origin", and each local branch should track a
  corresponding branch of the same name.
* Undoing a change should just work.

All that said, at the moment this is just a personal `git` wrapper tool, with one user. I encourage you to try running
it with the `debug=` environment variable to see the actual commands that are invoked, steal some ideas, write your own
tool, and tweak its behavior to your taste.

#### mit clone ≪repo≫

Clone a repo separately from its `.git` directory, so that the project folder structure can accomodate multiple
worktrees. Running `mit clone git@github.com:mitchellwrosen/mit.git` will create the following folder structure:

    mit/
      .git/
      master/


#### mit commit

Commit any uncommitted changes, and synchronize the current branch with its remote counterpart.

FIXME the details below are inaccurate, as everything is changing. Everything. Okay, not everything.

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

A `mit sync` effectively occurs whenever recording changes with `mit commit`.

FIXME the details below are inaccurate, as everything is changing. Everything. Okay, not everything.

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
