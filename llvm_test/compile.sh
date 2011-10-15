#!/bin/bash

./compile < $1
llc out.bc
as out.s -o out.o
gcc -c test.c
gcc test.o out.o -o test
