#!/bin/sh

FPCARCH=i386-linux
GCC=gcc-7
DST=../../static/$FPCARCH/sqlite3.o

rm $DST
rm sqlite3-$FPCARCH.o

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC
$GCC -static -O2 -m32 -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -c sqlite3mc.c -o sqlite3-$FPCARCH.o
cp sqlite3-$FPCARCH.o $DST

