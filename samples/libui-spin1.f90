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

    type(c_ptr) :: lbl1
    integer :: sValue

contains

  ! "close mainwindow" is a this app

  subroutine onClosing()
    use iso_c_binding
    use libuif
      call uiQuit()
  end subroutine onClosing

  subroutine onChange(s,d)
    use iso_c_binding
    use libuif
    implicit none
      type(c_ptr),value :: s,d
      character(len=22) :: s_value_string
      sValue = uiSpinboxValue(s)
      write(s_value_string,'(1X,"SPINの値は＝",I4)') sValue
      call uiLabelSetText(d, s_value_string//c_null_char)
  end subroutine onChange

  subroutine reset(s,d)
    use iso_c_binding
    use libuif
      type(c_ptr),value :: s,d
      call uiSpinboxSetValue(d,0)
      call uiLabelSetText(lbl1, "スピンの値"//c_null_char)
  end subroutine reset

end module handlers

program main
    use iso_c_binding
    use libuif
    use handlers

    implicit none
    type(c_ptr) :: mainwin
    type(c_ptr) :: vbox,btn1,ebtn,spin

! initialize
  call uiInit(0)

! start procedure

  mainwin = uiNewWindow("libui Simple Window"//c_null_char, 200, 100, 0)
  call uiWindowSetMargined(mainwin, 1)
  call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)

  vbox = uiNewVerticalBox()
  call uiWindowSetChild(mainwin, vbox)

  lbl1 = uiNewLabel("スピンの値"//c_null_char)
  spin = uiNewSpinbox(0,100)
  call uiSpinboxOnChanged(spin, c_funloc(onChange), lbl1)
  btn1 = uiNewButton("リセット"//c_null_char)
  call uiButtonOnClicked(btn1, c_funloc(reset), spin)
  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)

  call uiBoxAppend(vbox, lbl1, 1)
  call uiBoxAppend(vbox, spin, 1)
  call uiBoxAppend(vbox, btn1, 0)
  call uiBoxAppend(vbox, ebtn, 0)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main
