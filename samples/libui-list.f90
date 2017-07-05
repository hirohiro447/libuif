!
! libui-list.f90
! Fortran から　libui　を使う。
!
! Author Yoji.Hosokawa 2016/4
!
! ex.
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
    type(c_ptr) :: vbox,ebtn,cbox,ecbox

! initialize
  call uiInit(0)

! start procedure

  mainwin = uiNewWindow("Libui-List"//c_null_char, 300, 100, 1)
  call uiWindowSetMargined(mainwin, 1)
  call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)

  vbox = uiNewVerticalBox()
  call uiBoxSetPadded(vbox, 1)
  call uiWindowSetChild(mainwin, vbox)

  cbox = uiNewCombobox()
  call uiComboboxAppend(cbox, "Combobox Item 1"//c_null_char)
  call uiComboboxAppend(cbox, "Combobox Item 2"//c_null_char)
  call uiComboboxAppend(cbox, "Combobox Item 3"//c_null_char)
  call uiBoxAppend(vbox, cbox, 0)

  ecbox = uiNewEditableCombobox()
  call uiEditableComboboxAppend(ecbox, "Editable Item 1"//c_null_char)
  call uiEditableComboboxAppend(ecbox, "Editable Item 2"//c_null_char)
  call uiEditableComboboxAppend(ecbox, "Editable Item 3"//c_null_char)
  call uiBoxAppend(vbox, ecbox, 0)

  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)
  call uiBoxAppend(vbox, ebtn, 0)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main

