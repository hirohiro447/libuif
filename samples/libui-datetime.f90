!
! libui-datetime.f90
! Fortran から　libui　を使う。
!
! Author Yoji.Hosokawa 2016/4
!
! gfortran -O2 -g -Wall -o libui-window libui-window.f90 -L.-lui `pkg-config gtk+-3.0 --libs`
!

module handlers
  use iso_c_binding
  implicit none

contains

  ! "close mainwindow" is a this app

  subroutine onClosing()
    use iso_c_binding
    use libuif
      call uiQuit()
  end subroutine onClosing

end module handlers

program main
    use iso_c_binding
    use libuif
    use handlers

    implicit none
    type(c_ptr) :: mainwin
    type(c_ptr) :: vbox,ebtn

! initialize
  call uiInit(0)

! start procedure

  mainwin = uiNewWindow("libui Date Time"//c_null_char, 200, 200, 0)
  call uiWindowSetMargined(mainwin, 1)
  call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)

  vbox = uiNewVerticalBox()
  call uiWindowSetChild(mainwin, vbox)

  call uiBoxAppend(vbox,uiNewDatePicker(),0)
  call uiBoxAppend(vbox,uiNewTimePicker(),0)
  call uiBoxAppend(vbox,uiNewDateTimePicker(),0)

  call uiBoxAppend(vbox,uiNewHorizontalSeparator(),0)

  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)

  call uiBoxAppend(vbox, ebtn, 0)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main
