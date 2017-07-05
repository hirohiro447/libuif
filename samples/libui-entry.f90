!
! libui-entry.f90
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
    type(c_ptr) :: vbox,ebtn,group,entryForm

! initialize
  call uiInit(0)

! start procedure

  mainwin = uiNewWindow("Libui-Entry"//c_null_char, 300, 300, 1)
  call uiWindowSetMargined(mainwin, 1)
  call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)

  vbox = uiNewVerticalBox()
  call uiBoxSetPadded(vbox, 1)
  call uiWindowSetChild(mainwin, vbox)

  group = uiNewGroup("Entries"//c_null_char)
  call uiGroupSetMargined(group, 1)
  call uiBoxAppend(vbox, group, 1)

  entryForm = uiNewForm()
  call uiFormSetPadded(entryForm, 1)
  call uiGroupSetChild(group, entryForm)

  call uiFormAppend(entryForm,"Entry"//c_null_char,uiNewEntry(),0)
  call uiFormAppend(entryForm,"Password Entry"//c_null_char,uiNewPasswordEntry(),0)
  call uiFormAppend(entryForm,"Search Entry"//c_null_char,uiNewSearchEntry(),0)
  call uiFormAppend(entryForm,"Multiline Entry"//c_null_char,uiNewMultilineEntry(),1)
  call uiFormAppend(entryForm,"Multiline Entry No Wrap"//c_null_char,uiNewNonWrappingMultilineEntry(),1)

  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)
  call uiBoxAppend(vbox, ebtn, 0)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main

