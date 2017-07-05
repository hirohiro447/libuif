# libuif

## Overview

Fortran module for libui: a portable GUI library for C

libui: https://github.com/andlabs/libui

## Requirement

libui: https://github.com/andlabs/libui

gfortran Version4.9 later

## Update

2017/7/5  add C interface usleep and pthreads, add samples

## Usage

Linux

ex.

   gfortran -c -O2 -g -Wall libuif_structs.f90

   gfortran -c -O2 -g -Wall libuif.f90

 
puts sample source and libuif_structs.mod,libuif.mod and libui.a(if use static link library) in same folder.

  gfortran -O2 -g -Wall -o ./sample sample-source.f90 -L. -lui `pkg-config gtk+-3.0 --libs` -ldl


## Licence

[MIT](https://github.com/hirohiro447/libuif/blob/master/LICENSE)

## Author

[hirohiro447](https://github.com/hirohiro447)
