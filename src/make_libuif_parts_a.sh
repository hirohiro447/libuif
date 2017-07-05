#!/bin/sh

gcc -c -Wall pthreads.c
gfortran -c -Wall libuif_parts.f90

ar cr libuif_parts.a pthreads.o libuif_parts.o

