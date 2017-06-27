!
! libui-window.f90
! Fortran and libui
!
! Author Yoji.Hosokawa 2016/4
!
! gfortran -O2 -g -Wall -o libui-window libui-window.f90 -L.-lui `pkg-config gtk+-3.0 --libs`
!

module libuif
  use iso_c_binding
  implicit none

interface

  function uiNewWindow(title, width, height, hasMenubar) result(ret) BIND(C, name='uiNewWindow')
    use iso_c_binding
    character(kind=c_char), dimension(*), intent(in) :: title
    integer( kind = c_int ), value :: width, height, hasMenubar
    type(c_ptr) :: ret
  end function uiNewWindow


  subroutine uiInit(options) BIND(C, name='uiInit')
    use iso_c_binding
    integer(c_int) :: options
  end subroutine uiInit

  subroutine uiWindowSetMargined(w, margined) BIND(C, name='uiWindowSetMargined')
    use iso_c_binding
    type(c_ptr), value :: w
    integer( kind = c_int ), value :: margined
  end subroutine uiWindowSetMargined

  subroutine uiWindowOnClosing(w, f, d) BIND(C, name='uiWindowOnClosing')
    use iso_c_binding
    type(c_ptr), value :: w
    type(c_funptr), value :: f
    type(c_ptr), value :: d
  end subroutine uiWindowOnClosing

  subroutine uiQuit() BIND(C, name='uiQuit')
    use iso_c_binding
  end subroutine uiQuit

  subroutine uiControlShow(w) BIND(C, name='uiControlShow')
    use iso_c_binding
    type(c_ptr), value :: w
  end subroutine uiControlShow

  subroutine uiMain() BIND(C, name='uiMain')
    use iso_c_binding
  end subroutine uiMain

  subroutine uiUninit() BIND(C, name='uiUninit')
    use iso_c_binding
  end subroutine uiUninit

end interface
  
contains

  ! "close mainwindow" is a this app

  subroutine onClosing()
    use iso_c_binding
      call uiQuit()
  end subroutine onClosing

end module libuif

program main
    use iso_c_binding
    use libuif

    implicit none
    type( c_ptr ) :: mainwin

! initialize
  call uiInit(0)

! start procedure    
 
  mainwin = uiNewWindow("libui Simple Window"//c_null_char, 640, 480, 1)
  call uiWindowSetMargined(mainwin, 1)
  call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)
   
  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main

