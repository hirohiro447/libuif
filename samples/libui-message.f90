!
! libui-hslider.f90
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

  subroutine onMsgBoxClicked(s,d)
    use iso_c_binding
    use libuif
      type(c_ptr),value :: s,d
      call uiMsgBox(d,"ノーマルメッセージボックス"//c_null_char, &
&          "More detailed information can be shown here."//c_null_char)
  end subroutine onMsgBoxClicked

  subroutine onMsgBoxErrorClicked(s,d)
    use iso_c_binding
    use libuif
      type(c_ptr),value :: s,d
      call uiMsgBoxError(d,"エラーメッセージボックス"//c_null_char, &
&          "More detailed information can be shown here."//c_null_char)
  end subroutine onMsgBoxErrorClicked

end module handlers

program main
    use iso_c_binding
    use libuif
    use handlers

    implicit none
    type(c_ptr) :: mainwin
!    type(c_ptr) :: grid
    type(c_ptr) :: vbox,msggrid,button,ebtn

! initialize
  call uiInit(0)

! start procedure

  mainwin = uiNewWindow("libui MessageBox"//c_null_char, 200, 100, 0)
  call uiWindowSetMargined(mainwin, 1)
  call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)

  vbox = uiNewVerticalBox()
  call uiBoxSetPadded(vbox, 1)
  call uiWindowSetChild(mainwin, vbox)

!  grid = uiNewGrid()
!  call uiGridSetPadded(grid, 1)
!  call uiBoxAppend(vbox, grid, 0)

  msggrid = uiNewGrid()
  call uiGridSetPadded(msggrid, 1)
!  call uiGridAppend(grid, msggrid,0, 2, 2, 1,0, uiAlignCenter, 0, uiAlignStart)
  call uiBoxAppend(vbox, msggrid, 0)

  button = uiNewButton("Message Box"//c_null_char)
  call uiButtonOnClicked(button, c_funloc(onMsgBoxClicked), mainwin)
  call uiGridAppend(msggrid, button,0, 0, 1, 1,0, uiAlignFill, 0, uiAlignFill)

  button = uiNewButton("Error Box"//c_null_char)
  call uiButtonOnClicked(button, c_funloc(onMsgBoxErrorClicked), mainwin)
  call uiGridAppend(msggrid, button,1, 0, 1, 1,0, uiAlignFill, 0, uiAlignFill)

  call uiBoxAppend(vbox,uiNewHorizontalSeparator(),1)

  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)

  call uiBoxAppend(vbox, ebtn, 0)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main
