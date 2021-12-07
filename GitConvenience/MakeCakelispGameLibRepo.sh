#!/bin/sh

git init

mkdir src
mkdir Dependencies
git submodule add git@github.com:makuto/cakelisp.git Dependencies/cakelisp
git submodule add gitea@macoy.me:macoy/gamelib.git Dependencies/gamelib

cp Dependencies/cakelisp/COPYING .
cp Dependencies/cakelisp/LICENSE .
touch ReadMe.org

git add .
git commit -m"Initial commit"
