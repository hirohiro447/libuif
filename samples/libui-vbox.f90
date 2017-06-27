!
! libui-vbox.f90
! Fortran から　libui　を使う。
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

    type(c_ptr) :: lbl1

contains

  ! "close mainwindow" is a this app

  subroutine onClosing()
    use iso_c_binding
      call uiQuit()
  end subroutine onClosing

  subroutine lbl_up()
    use iso_c_binding
      call uiLabelSetText(lbl1, "pushed pushed pushed"//c_null_char)
  end subroutine lbl_up

end module handlers

program main
    use iso_c_binding
    use libuif
    use handlers

    implicit none
    type(c_ptr) :: mainwin
    type(c_ptr) :: vbox,btn1,ebtn

! initialize
  call uiInit(0)

! start procedure    
 
  mainwin = uiNewWindow("VBOX"//c_null_char, 50, 100, 1)
  call uiWindowSetMargined(mainwin, 1)
  call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)
  
  vbox = uiNewVerticalBox()
  call uiBoxSetPadded(vbox, 1)
  call uiWindowSetChild(mainwin, vbox)

  lbl1 = uiNewLabel("This is Label"//c_null_char)
  btn1 = uiNewButton("This is Button"//c_null_char)
  call uiButtonOnClicked(btn1, c_funloc(lbl_up), c_null_ptr)
  ebtn = uiNewButton("Quit"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)

  call uiBoxAppend(vbox, lbl1, 1)
  call uiBoxAppend(vbox, btn1, 1)
  call uiBoxAppend(vbox, ebtn, 1)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main

