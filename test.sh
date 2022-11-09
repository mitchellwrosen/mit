#!/bin/bash

# FIXME use some tmp dir

# FIXME write tests that hit all types of conflicts

rm -rf scrap
mkdir -p scrap/remote
mkdir scrap/local

cd scrap/remote
git init --initial-branch=master >/dev/null || exit 1
touch one.txt
touch two.txt
touch three.txt
git add --all || exit 1
git commit -m 'initial commit' >/dev/null || exit 1

git switch --create feature >/dev/null 2>&1 || exit 1
echo two > two.txt
git commit --all --message two >/dev/null || exit 1

echo three > three.txt
git commit --all --message three >/dev/null || exit 1

git switch master >/dev/null 2>&1 || exit 1

cd ..
git clone remote local >/dev/null 2>&1 || exit 1

cd local
git switch --create feature >/dev/null 2>&1 || exit 1
git branch --set-upstream-to origin/feature >/dev/null || exit 1
git reset --hard origin/feature >/dev/null || exit 1

echo "mit merge"
echo "========="
echo " | branch   | merge        | working tree |"
echo " | -------------------------------------- |"

echo " | ahead    | none         | clean        |"
git reset --hard origin/feature >/dev/null || exit 1
echo four >> three.txt
git commit --all --message "four" >/dev/null || exit 1
head0=$(git rev-parse feature)
mit merge feature >/dev/null || exit 1
[ $(git rev-parse feature) = $head0 ] || exit 1
mit undo >/dev/null && exit 1

echo " | ahead    | none         | dirty        |"
git reset --hard origin/feature >/dev/null || exit 1
echo four >> three.txt
git commit --all --message "four" >/dev/null || exit 1
echo five >> three.txt
head0=$(git rev-parse feature)
mit merge feature >/dev/null || exit 1
[ $(git rev-parse feature) = $head0 ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1
mit undo >/dev/null && exit 1

echo " | diverged | bubble       | clean        |"
git reset --hard origin/feature^ >/dev/null || exit 1
echo three >> two.txt
git commit --all --message "three" >/dev/null || exit 1
head0=$(git rev-parse feature)
mit merge feature >/dev/null || exit 1
[ $(git rev-parse feature^) = $head0 ] || exit 1
[ $(git rev-parse feature^2) = $(git rev-parse origin/feature) ] || exit 1
mit undo >/dev/null || exit 1
[ $(git rev-parse feature) = $head0 ] || exit 1

echo " | diverged | bubble       | dirty        |"
git reset --hard origin/feature^ >/dev/null || exit 1
echo three >> two.txt
git commit --all --message "three" >/dev/null || exit 1
echo four >> two.txt
head0=$(git rev-parse feature)
mit merge feature >/dev/null || exit 1
[ $(git rev-parse feature^) = $head0 ] || exit 1
[ $(git rev-parse feature^2) = $(git rev-parse origin/feature) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1
mit undo >/dev/null || exit 1
[ $(git rev-parse feature) = $head0 ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1

echo " | diverged | bubble       | conflicting  |"
git reset --hard origin/feature^ >/dev/null || exit 1
echo three >> two.txt
git commit --all --message "three" >/dev/null || exit 1
echo four >> three.txt
head0=$(git rev-parse feature)
mit merge feature >/dev/null || exit 1
[ $(git rev-parse feature^) = $head0 ] || exit 1
[ $(git rev-parse feature^2) = $(git rev-parse origin/feature) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 5 insertions(+)" ] || exit 1
mit undo >/dev/null || exit 1
[ $(git rev-parse feature) = $head0 ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1

echo " | diverged | conflicting  | clean        |"
git reset --hard origin/feature^ >/dev/null || exit 1
echo four >> three.txt
git commit --all --message "four" >/dev/null || exit 1
head0=$(git rev-parse feature)
mit merge feature >/dev/null || exit 1
[ $(git rev-parse feature) = $head0 ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 5 insertions(+)" ] || exit 1
mit undo >/dev/null || exit 1
[ $(git rev-parse feature) = $head0 ] || exit 1

echo " | diverged | conflicting  | dirty        |"
git reset --hard origin/feature^ >/dev/null || exit 1
echo four >> three.txt
git commit --all --message "four" >/dev/null || exit 1
echo three >> two.txt
head0=$(git rev-parse feature)
mit merge feature >/dev/null || exit 1
[ $(git rev-parse feature) = $head0 ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 5 insertions(+)" ] || exit 1
mit undo >/dev/null || exit 1
[ $(git rev-parse feature) = $head0 ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1

echo " | diverged | conflicting  | conflicting  |"

echo "mit sync: local = remote, no changes"
git reset --hard origin/feature >/dev/null || exit 1
mit sync >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
mit undo >/dev/null && exit

echo "mit sync: local = remote, changes"
echo three >> three.txt
mit sync >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1
mit undo >/dev/null && exit 1

echo "mit sync: local behind remote, no changes"
git reset --hard origin/feature^ >/dev/null || exit 1
mit sync >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
mit undo >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature^) ] || exit 1

echo "mit sync: local behind remote, changes (conflicts)"
git reset --hard origin/feature^ >/dev/null || exit 1
echo two >> two.txt
mit sync >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1
mit undo >/dev/null || exit 1
[ $(git rev-parse feature) = "$(git rev-parse origin/feature^)" ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1

echo "mit sync: local behind remote, changes (no conflicts)"
git reset --hard origin/feature^ >/dev/null || exit 1
echo four > three.txt
mit sync >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 5 insertions(+)" ] || exit 1
mit undo >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature^) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1

echo "mit sync: local ahead of remote, no changes"
git reset --hard origin/feature >/dev/null || exit 1
head=$(git rev-parse HEAD)
echo two >> two.txt
git commit --all --message two >/dev/null || exit 1
mit sync >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
mit undo >/dev/null && exit 1
git reset --hard HEAD^ >/dev/null || exit 1
git push --force >/dev/null 2>&1 || exit 1

echo "mit sync: local ahead of remote, changes"
git reset --hard origin/feature >/dev/null || exit 1
head=$(git rev-parse HEAD)
echo two >> two.txt
git commit --all --message two >/dev/null || exit 1
echo three >> two.txt
mit sync >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1
mit undo >/dev/null && exit 1
git reset --hard HEAD^ >/dev/null || exit 1
git push --force >/dev/null 2>&1 || exit 1

echo "TODO mit sync: local diverged from remote (no conflicts), no changes"
echo "TODO mit sync: local diverged from remote (no conflicts), changes (no conflicts)"
echo "TODO mit sync: local diverged from remote (no conflicts), changes (conflicts)"
echo "TODO mit sync: local diverged from remote (conflicts), no changes"
echo "TODO mit sync: local diverged from remote (conflicts), changes (no conflicts)"
echo "TODO mit sync: local diverged from remote (conflicts), changes (conflicts)"

echo "mit commit: local = remote"
git reset --hard origin/feature >/dev/null || exit 1
head=$(git rev-parse HEAD)
echo three >> three.txt
MIT_COMMIT_MESSAGE=three mit commit </dev/null >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
[ $head = $(git rev-parse feature^) ] || exit 1
mit undo >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
[ $head = $(git rev-parse feature^^) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1
git reset --hard HEAD^^ >/dev/null || exit 1
git push --force >/dev/null 2>&1 || exit 1

echo "mit commit: local = remote, commit aborted"
git reset --hard origin/feature >/dev/null || exit 1
echo three >> three.txt
mit commit </dev/null >/dev/null || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1
mit undo >/dev/null && exit 1

echo "mit commit: local behind remote"
git reset --hard origin/feature^ >/dev/null || exit 1
local0=$(git rev-parse feature)
remote0=$(git rev-parse origin/feature)
echo two >> two.txt
MIT_COMMIT_MESSAGE=two mit commit </dev/null >/dev/null && exit 1
MIT_COMMIT_MESSAGE=two mit commit </dev/null >/dev/null || exit 1
[ $local0 = $(git rev-parse feature^) ] || exit 1
[ $remote0 = $(git rev-parse origin/feature) ] || exit 1
mit undo >/dev/null || exit 1
[ $local0 = $(git rev-parse feature) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1

set +x
echo "mit commit: local ahead of remote"
git reset --hard origin/feature >/dev/null || exit 1
head=$(git rev-parse HEAD)
git push origin --force HEAD^:feature >/dev/null 2>&1 || exit 1
echo three >> three.txt
MIT_COMMIT_MESSAGE=three mit commit </dev/null >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
[ $head = $(git rev-parse feature^) ]
mit undo >/dev/null && exit 1
git reset --hard $head >/dev/null || exit 1
git push --force >/dev/null 2>&1 || exit 1

echo "mit commit: local ahead of remote, commit aborted"
git reset --hard origin/feature >/dev/null || exit 1
head=$(git rev-parse HEAD)
git push origin --force HEAD^:feature >/dev/null 2>&1 || exit 1
echo three >> three.txt
mit commit </dev/null >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1
mit undo >/dev/null && exit 1
git reset --hard $head >/dev/null || exit 1
git push --force >/dev/null 2>&1 || exit 1

echo "mit commit: local diverged from remote"
git reset --hard origin/feature^ >/dev/null || exit 1
upstream0=$(git rev-parse origin/feature)
echo two >> two.txt
git commit --all --message two >/dev/null || exit 1
local0=$(git rev-parse feature)
echo two >> two.txt
MIT_COMMIT_MESSAGE=two mit commit </dev/null >/dev/null || exit 1
[ $upstream0 = $(git rev-parse origin/feature) ] || exit 1
[ $local0 = $(git rev-parse feature^) ] || exit 1
mit undo >/dev/null || exit 1
[ $local0 = $(git rev-parse feature) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1

echo "mit commit: local diverged from remote, commit aborted"
git reset --hard origin/feature^ >/dev/null || exit 1
upstream0=$(git rev-parse origin/feature)
echo two >> two.txt
git commit --all --message two >/dev/null || exit 1
local0=$(git rev-parse feature)
echo two >> two.txt
mit commit </dev/null >/dev/null || exit 1
[ $upstream0 = $(git rev-parse origin/feature) ] || exit 1
[ $local0 = $(git rev-parse feature) ] || exit 1
mit undo >/dev/null && exit 1

cd ../..
rm -rf scrap
