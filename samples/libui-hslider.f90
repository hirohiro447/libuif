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
  use libuif
  implicit none

    type(c_ptr) :: lbl1, slider
    integer :: sValue

contains

  ! "close mainwindow" is a this app

  subroutine onClosing()
    use iso_c_binding
      call uiQuit()
  end subroutine onClosing

  subroutine onChange()
    use iso_c_binding
    implicit none
      character(len=5) :: s_value_string

      sValue = uiSliderValue(slider)
      write(s_value_string,'(I5)') sValue
      call uiLabelSetText(lbl1, s_value_string//c_null_char)
  end subroutine onChange

  subroutine reset()
    use iso_c_binding
      call uiSliderSetValue(slider,0)
      call uiLabelSetText(lbl1, "スライダーの値"//c_null_char)
  end subroutine reset

end module handlers

program main
    use iso_c_binding
    use libuif
    use handlers

    implicit none
    type(c_ptr) :: mainwin
    type(c_ptr) :: hbox,btn1,ebtn

! initialize
  call uiInit(0)

! start procedure    
 
  mainwin = uiNewWindow("libui Simple Window"//c_null_char, 400, 40, 0)
  call uiWindowSetMargined(mainwin, 1)
  call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)
  
  hbox = uiNewHorizontalBox()
  call uiWindowSetChild(mainwin, hbox)

  lbl1 = uiNewLabel("スライダーの値"//c_null_char)
  slider = uiNewSlider(0,100)
  call uiSliderOnChanged(slider, c_funloc(onChange), c_null_ptr)
  btn1 = uiNewButton("リセット"//c_null_char)
  call uiButtonOnClicked(btn1, c_funloc(reset), c_null_ptr)
  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)

  call uiBoxAppend(hbox, lbl1, 1)
  call uiBoxAppend(hbox, slider, 1)
  call uiBoxAppend(hbox, btn1, 0)
  call uiBoxAppend(hbox, ebtn, 0)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main

