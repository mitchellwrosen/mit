# mit

`mit` is a wrapper around `git` that aims to improve its UX by exposing high-level commands, synchronizing with the
remote repository automatically, providing informative and visual feedback, and allowing the user to undo the last
modification whenever possible.

Compared to `git`, which encourages siloed, offline development until an acceptable level of quality and completeness is
reached and commit history rewritten to show a pristine path, `mit` follows the general philosophy that development
should be iterative and highly collaborative, checkins should occur frequently, history should be preserved accurately,
and it is perfectly okay to have points in the history that do not even build, let alone work correctly.

## The commands

### `mit branch`

```mermaid
graph TD
  mit_branch[["mit branch #lt;branch#gt;"]] --> branch_has_worktree?["let #lt;branchdir#gt; = #lt;rootdir#gt;/#lt;branch#gt;\nIs #lt;branch#gt; already checked out in a worktree?"]
  style branch_has_worktree? text-align:left
  branch_has_worktree? -->|No| branchdir_exists?["Does #lt;branchdir#gt; already exist?"]
  branchdir_exists? -->|No| branch_exists?["Create detached worktree at #lt;branchdir#gt;\ncd #lt;branchdir#gt;\nDoes #lt;branch#gt; exist?"]
  style branch_exists? text-align:left
  branch_exists? -->|No| upstream_exists?["Create #lt;branch#gt;\nFetch origin\nlet #lt;upstream#gt; = origin/#lt;branch#gt;\nDoes #lt;upstream#gt; exist?"]
  style upstream_exists? text-align:left
  upstream_exists? -->|No| default_exists?["Does origin have a default branch?"]
  default_exists? -->|No| done["Done"]
  default_exists? -->|"Yes, #lt;default#gt;"| point_to_def["Point to #lt;default#gt;"]
  point_to_def --> done
  upstream_exists? -->|Yes| track_upstream["Point to #lt;upstream#gt;\nTrack #lt;upstream#gt;"]
  style track_upstream text-align:left
  track_upstream --> done
  branch_exists? -->|Yes| switch_to_branch["Switch to #lt;branch#gt;"]
  switch_to_branch --> done
  branchdir_exists? -->|Yes| branchdir_exists["#lt;branchdir#gt; already exists"]
  style branchdir_exists color:red
  branch_has_worktree? -->|"Yes, #lt;dir#gt;"| branch_worktree_is_branchdir?["Is #lt;dir#gt; the same as #lt;branchdir#gt;?"]
  branch_worktree_is_branchdir? -->|No| branch_already_has_worktree["#lt;branch#gt; is already checked out in #lt;dir#gt;"]
  style branch_already_has_worktree color:red
  branch_worktree_is_branchdir? --->|Yes| done
  style done color:green
```

### `mit commit`

TODO document this

### `mit merge`

TODO document this

### `mit sync`

TODO document this

### `mit undo`

TODO document this
