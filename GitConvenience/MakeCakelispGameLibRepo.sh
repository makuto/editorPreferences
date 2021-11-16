#!/bin/sh

mkdir src
mkdir Dependencies
cd Dependencies
git clone git@github.com:makuto/cakelisp.git
git clone gitea@macoy.me:macoy/gamelib.git
cd ../

cp Dependencies/cakelisp/COPYING .
cp Dependencies/cakelisp/LICENSE .
touch ReadMe.org
