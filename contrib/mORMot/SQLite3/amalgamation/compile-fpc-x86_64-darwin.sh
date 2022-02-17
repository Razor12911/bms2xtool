#!/bin/sh

ARCH=x86_64-darwin
DST=../../static/$ARCH/sqlite3.o

CROSS=/home/ab/fpcup/cross
SDK=$CROSS/lib/x86-darwin/MacOSX10.11.sdk\usr
GCC=$CROSS/bin/x86-darwin/x86_64-apple-darwin15

rm $DST
rm sqlite3-$ARCH.o

echo
echo ---------------------------------------------------
echo Compiling for FPC on $ARCH using $GCC
$GCC-clang -static -target x86_64-apple-darwin15 -O2 -m64 -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -I$SDK/include -c sqlite3mc.c -o sqlite3-$ARCH.o
cp sqlite3-$ARCH.o $DST

$GCC-libtool -static sqlite3-$ARCH.o -o ../../static/$ARCH/libsqlite3.a
