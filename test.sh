#!/bin/bash

# FIXME use some tmp dir

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
git commit -a -m 'two' >/dev/null || exit 1

echo three > three.txt
git commit -a -m 'three' >/dev/null || exit 1

git switch master >/dev/null 2>&1 || exit 1

cd ..
git clone remote local >/dev/null 2>&1 || exit 1

cd local
git switch --create feature >/dev/null 2>&1 || exit 1
git branch --set-upstream-to origin/feature >/dev/null || exit 1
git reset --hard origin/feature >/dev/null || exit 1

#
# * three  (feature, origin/feature)
# |
# * two
# |
# * one
#
echo "sync: local = remote, no changes"
mit sync >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
mit undo >/dev/null && exit 1

#
# + three  (feature, origin/feature)
# |
# * two
# |
# * one
#
echo "sync: local = remote, changes"
echo three >> three.txt
mit sync >/dev/null || exit 1
[ "$(git rev-parse feature)" = "$(git rev-parse origin/feature)" ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1
mit undo >/dev/null && exit 1

#
# * three  (origin/feature)
# |
# * two    (feature)
# |
# * one
#
echo "sync: local behind remote, no changes"
git reset --hard origin/feature^ >/dev/null || exit 1
mit sync >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
mit undo >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature^) ] || exit 1

#
# * three  (origin/feature)
# |
# + two    (feature)
# |
# * one
#
echo "sync: local behind remote, nonconflicting changes"
git reset --hard origin/feature^ >/dev/null || exit 1
echo two >> two.txt
mit sync >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1
mit undo >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature^) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1

#
# * three  (origin/feature)
# |
# + two    (feature)
# |
# * one
#
echo "sync: local behind remote, conflicting changes"
git reset --hard origin/feature^ >/dev/null || exit 1
echo four > three.txt
mit sync >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 5 insertions(+)" ] || exit 1
mit undo >/dev/null || exit 1
[ $(git rev-parse feature) = $(git rev-parse origin/feature^) ] || exit 1
[ "$(git diff --shortstat)" = " 1 file changed, 1 insertion(+)" ] || exit 1

rm -rf scrap
