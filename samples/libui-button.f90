!
! libui-window.f90
! Fortran and libui
!
! Author Yoji.Hosokawa 2016/4
!
! ex.
! gfortran -O2 -g -Wall -o libui-window libui-window.f90 -L.-lui `pkg-config gtk+-3.0 --libs`
!

module handlers
  use iso_c_binding
  use libuif
  implicit none

contains

  ! "close mainwindow" is a this app

  subroutine onClosing()
    use iso_c_binding
      call uiQuit()
  end subroutine onClosing

end module handlers

program main
    use iso_c_binding
    use libuif
    use handlers

    implicit none
    type(c_ptr) :: mainwin
    type(c_ptr) :: button

! initialize
  call uiInit(0)

! start procedure    
 
  mainwin = uiNewWindow("libui Simple Window"//c_null_char, 640, 480, 1)
  call uiWindowSetMargined(mainwin, 1)
  call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)
   
  button = uiNewButton("this is button"//c_null_char)
  call uiWindowSetChild(mainwin, button)



  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main

