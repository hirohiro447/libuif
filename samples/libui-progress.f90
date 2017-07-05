!
! libui-progress.f90
! Fortran から　libui　を使う。
!
! Author Yoji.Hosokawa 2016/4
!
! gfortran -O2 -g -Wall -o libui-window libui-window.f90 -L.-lui `pkg-config gtk+-3.0 --libs`
!

module handlers
  use iso_c_binding
!  use libuif
  implicit none

    type(c_ptr) :: spinbox,slider,pbar
!    integer :: sValue

contains

  ! "close mainwindow" is a this app

  subroutine onClosing()
    use iso_c_binding
    use libuif
      call uiQuit()
  end subroutine onClosing

  subroutine onSpinboxChanged()
    use iso_c_binding
    use libuif
    implicit none
      call uiSliderSetValue(slider, uiSpinboxValue(spinbox))
      call uiProgressBarSetValue(pbar, uiSpinboxValue(spinbox))
  end subroutine onSpinboxChanged

  subroutine onSliderChanged()
    use iso_c_binding
    use libuif
    implicit none
      call uiSpinboxSetValue(spinbox, uiSliderValue(slider))
      call uiProgressBarSetValue(pbar, uiSliderValue(slider))
  end subroutine onSliderChanged

end module handlers

program main
    use iso_c_binding
    use libuif
    use handlers

    implicit none
    type(c_ptr) :: mainwin
    type(c_ptr) :: vbox,ebtn,ip

! initialize
  call uiInit(0)

! start procedure

  mainwin = uiNewWindow("libui Progress Bar"//c_null_char, 200, 100, 0)
  call uiWindowSetMargined(mainwin, 1)
  call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)

  vbox = uiNewVerticalBox()
  call uiWindowSetChild(mainwin, vbox)

  spinbox = uiNewSpinbox(0, 100)
  slider = uiNewSlider(0, 100)
  pbar = uiNewProgressBar()
  call uiSpinboxOnChanged(spinbox, c_funloc(onSpinboxChanged), c_null_ptr)
  call uiSliderOnChanged(slider, c_funloc(onSliderChanged), c_null_ptr)

  ip = uiNewProgressBar()
  call uiProgressBarSetValue(ip,-1)

  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)

  call uiBoxAppend(vbox, spinbox, 1)
  call uiBoxAppend(vbox, slider, 1)
  call uiBoxAppend(vbox, pbar, 1)
  call uiBoxAppend(vbox, ip, 1)
  call uiBoxAppend(vbox, ebtn, 1)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main
