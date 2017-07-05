!
! libui-opensave.f90
! Fortran から　libui　を使う。
!
! Author Yoji.Hosokawa 2016/4
!
! gfortran -O2 -g -Wall -o libui-window libui-window.f90 -L.-lui `pkg-config gtk+-3.0 --libs`
!

module handlers
  use iso_c_binding
  implicit none

  type(c_ptr) :: mainwin
  
contains

  ! "close mainwindow" is a this app

  subroutine onClosing()
    use iso_c_binding
    use libuif
      call uiQuit()
  end subroutine onClosing

  subroutine onOpenFileClicked(b, d)
    use iso_c_binding
    use libuif

    type(c_ptr),value :: b,d
    type(c_ptr) :: filename_c
    integer :: i
    character(1000, C_CHAR),pointer :: filename_f

    filename_c = uiOpenFile(mainwin)
!    if (C_ASSOCIATED(filename_c) .eqv. .true.) then
!      call c_f_pointer(filename_c,filename_f)
!      i = index(filename_f,char(0))
!      call uiEntrySetText(d, filename_f(1:i))
!      call uiFreeText(filename_f)
!    else
!      call uiEntrySetText(d, "(cancelled)"//c_null_char)
!    end if

    if (C_ASSOCIATED(filename_c) .neqv. .true.) then
      call uiEntrySetText(d, "(cancelled)"//c_null_char)
    else
      call c_f_pointer(filename_c,filename_f)
      i = index(filename_f,char(0))
      call uiEntrySetText(d, filename_f(1:i))
      call uiFreeText(filename_f)
    end if

  end subroutine onOpenFileClicked
  
  subroutine onSaveFileClicked(b, d)
    use iso_c_binding
    use libuif

    type(c_ptr),value :: b,d
    type(c_ptr) :: filename_c
    integer :: i
    character(1000, C_CHAR),pointer :: filename_f

    filename_c = uiSaveFile(mainwin)
    if (C_ASSOCIATED(filename_c)) then
      call c_f_pointer(filename_c,filename_f)
      i = index(filename_f,char(0))
      call uiEntrySetText(d, filename_f(1:i))
      call uiFreeText(filename_f)
    else
      call uiEntrySetText(d, "(cancelled)"//c_null_char)
    end if

  end subroutine onSaveFileClicked

end module handlers

program main
    use iso_c_binding
    use libuif
    use handlers

    implicit none
!    type(c_ptr) :: mainwin
    type(c_ptr) :: vbox,ebtn,grid,button,entry1

! initialize
  call uiInit(0)

! start procedure

  mainwin = uiNewWindow("libui Open Save File"//c_null_char, 400, 100, 0)
  call uiWindowSetMargined(mainwin, 1)
  call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)

  vbox = uiNewVerticalBox()
  call uiWindowSetChild(mainwin, vbox)

  grid = uiNewGrid()
  call uiGridSetPadded(grid, 1)
  call uiBoxAppend(vbox, grid, 0)

  button = uiNewButton("Open File"//c_null_char)
  entry1 = uiNewEntry()
  call uiEntrySetReadOnly(entry1, 1)
  call uiButtonOnClicked(button, c_funloc(onOpenFileClicked), entry1);
  call uiGridAppend(grid, button,0, 0, 1, 1,0, uiAlignFill, 0, uiAlignFill)
  call uiGridAppend(grid, entry1,1, 0, 1, 1,1, uiAlignFill, 0, uiAlignFill)

  button = uiNewButton("Save File"//c_null_char)
  entry1 = uiNewEntry()
  call uiEntrySetReadOnly(entry1, 1)
  call uiButtonOnClicked(button, c_funloc(onSaveFileClicked), entry1)
  call uiGridAppend(grid, button,0, 1, 1, 1,0, uiAlignFill, 0, uiAlignFill)
  call uiGridAppend(grid, entry1,1, 1, 1, 1,1, uiAlignFill, 0, uiAlignFill)

  call uiBoxAppend(vbox,uiNewHorizontalSeparator(),0)

  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)

  call uiBoxAppend(vbox, ebtn, 0)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main
