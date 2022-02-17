#!/bin/sh

FPCARCH=i386-openbsd
FPCARCHVERSION=
CROSS=/home/ab/fpcup/cross/bin/$FPCARCH
GCC=$CROSS/$FPCARCH$FPCARCHVERSION-gcc
DST=../../static/$FPCARCH/sqlite3.o

rm $DST
rm sqlite3-$FPCARCH.o

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC
$GCC -static -O2 -m32 -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -c sqlite3mc.c -o sqlite3-$FPCARCH.o
cp sqlite3-$FPCARCH.o $DST

