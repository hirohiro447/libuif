!
! libui-mebu.f90
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
  type(c_ptr) :: entry1,entry2

  type(c_ptr) :: fileMenu
  type(c_ptr) :: newItem
  type(c_ptr) :: openItem
  type(c_ptr) :: saveItem
  type(c_ptr) :: shouldQuitItem
  type(c_ptr) :: quitItem
  type(c_ptr) :: editMenu
  type(c_ptr) :: undoItem
  type(c_ptr) :: checkItem
  type(c_ptr) :: accelItem
  type(c_ptr) :: prefsItem
  type(c_ptr) :: testMenu
  type(c_ptr) :: enabledItem
  type(c_ptr) :: enableThisItem
  type(c_ptr) :: forceCheckedItem
  type(c_ptr) :: forceUncheckedItem
  type(c_ptr) :: whatWindowItem
  type(c_ptr) :: moreTestsMenu
  type(c_ptr) :: quitEnabledItem
  type(c_ptr) :: prefsEnabledItem
  type(c_ptr) :: aboutEnabledItem
  type(c_ptr) :: checkEnabledItem
  type(c_ptr) :: multiMenu
  type(c_ptr) :: helpMenu
  type(c_ptr) :: helpItem
  type(c_ptr) :: aboutItem
  
contains

  ! "close mainwindow" is a this app

  subroutine onClosing()
    use iso_c_binding
    use libuif
      call uiQuit()
  end subroutine onClosing

  function onShouldQuit(d) result(ret)
    use iso_c_binding
    use libuif
      type(c_ptr), value :: d
      integer :: ret
        if (uiMenuItemChecked(shouldQuitItem) == 1) then
          call uiControlDestroy(d)
          ret = 1
          return
        end if
      ret = 0
  end function onShouldQuit

  subroutine onOpenFileClicked()
    use iso_c_binding
    use libuif

    type(c_ptr) :: filename_c
    integer :: i
    character(1000, C_CHAR),pointer :: filename_f

    filename_c = uiOpenFile(mainwin)
    if (C_ASSOCIATED(filename_c) .neqv. .true.) then
      call uiEntrySetText(entry1, "(cancelled)"//c_null_char)
    else
      call c_f_pointer(filename_c,filename_f)
      i = index(filename_f,char(0))
      call uiEntrySetText(entry1, filename_f(1:i))
      call uiFreeText(filename_f)
    end if

  end subroutine onOpenFileClicked
  
  subroutine onSaveFileClicked()
    use iso_c_binding
    use libuif

    type(c_ptr) :: filename_c
    integer :: i
    character(1000, C_CHAR),pointer :: filename_f

    filename_c = uiSaveFile(mainwin)
    if (C_ASSOCIATED(filename_c)) then
      call c_f_pointer(filename_c,filename_f)
      i = index(filename_f,char(0))
      call uiEntrySetText(entry2, filename_f(1:i))
      call uiFreeText(filename_f)
    else
      call uiEntrySetText(entry2, "(cancelled)"//c_null_char)
    end if

  end subroutine onSaveFileClicked

  subroutine onMsgBoxClicked()
    use iso_c_binding
    use libuif
      call uiMsgBox(mainwin,"このアプリについて"//c_null_char, &
&          "これはlibuiとFortranを使って作成した" // char(13) // 'ＧＵＩのサンプルです。'//c_null_char)
  end subroutine onMsgBoxClicked

  subroutine enableItemTest(item, w, d)
    use iso_c_binding
    use libuif
      type(c_ptr),value :: item, w, d

      if (uiMenuItemChecked(item) == 1) then
        call uiMenuItemEnable(d)
      else
        call uiMenuItemDisable(d)
      end if
  end subroutine enableItemTest

  subroutine forceOn()
    use iso_c_binding
    use libuif
      call uiMenuItemSetChecked(enabledItem, 1)
  end subroutine forceOn

  subroutine forceOff()
    use iso_c_binding
    use libuif
      call uiMenuItemSetChecked(enabledItem, 0)
  end subroutine forceOff

  subroutine whatWindow()
    use iso_c_binding
    use libuif
        print *, "menu item clicked on window %p\n"
  end subroutine whatWindow

  subroutine initMenus()
    use iso_c_binding
    use libuif

    type(c_ptr) :: dummy

      fileMenu = uiNewMenu("File"//c_null_char)
      newItem = uiMenuAppendItem(fileMenu, "New"//c_null_char)
      openItem = uiMenuAppendItem(fileMenu, "Open"//c_null_char)
      call uiMenuItemOnClicked(openItem, c_funloc(onOpenFileClicked), c_null_ptr)
      saveItem = uiMenuAppendItem(fileMenu, "保存"//c_null_char)
      call uiMenuItemOnClicked(saveItem, c_funloc(onSaveFileClicked), c_null_ptr)
      call uiMenuAppendSeparator(fileMenu)
      shouldQuitItem = uiMenuAppendCheckItem(fileMenu, "Should Quit"//c_null_char)
      quitItem = uiMenuAppendQuitItem(fileMenu)

      editMenu = uiNewMenu("Edit"//c_null_char)
      undoItem = uiMenuAppendItem(editMenu, "Undo"//c_null_char)
      call uiMenuItemDisable(undoItem)
      call uiMenuAppendSeparator(editMenu)
      checkItem = uiMenuAppendCheckItem(editMenu, "Check Me\tTest"//c_null_char)
      accelItem = uiMenuAppendItem(editMenu, "A&ccele&&rator T_es__t"//c_null_char)
      prefsItem = uiMenuAppendPreferencesItem(editMenu)

      testMenu = uiNewMenu("Test"//c_null_char)
      enabledItem = uiMenuAppendCheckItem(testMenu, "Enable Below Item"//c_null_char)
      call uiMenuItemSetChecked(enabledItem, 1)
      enableThisItem = uiMenuAppendItem(testMenu, "This Will Be Enabled"//c_null_char)
      call uiMenuItemOnClicked(enabledItem, c_funloc(enableItemTest), enableThisItem)
      forceCheckedItem = uiMenuAppendItem(testMenu, "Force Above Checked"//c_null_char)
      call uiMenuItemOnClicked(forceCheckedItem, c_funloc(forceOn), c_null_ptr)
      forceUncheckedItem = uiMenuAppendItem(testMenu, "Force Above Unchecked"//c_null_char)
      call uiMenuItemOnClicked(forceUncheckedItem, c_funloc(forceOff), c_null_ptr)
      call uiMenuAppendSeparator(testMenu)
      whatWindowItem = uiMenuAppendItem(testMenu, "What Window?"//c_null_char)
      call uiMenuItemOnClicked(whatWindowItem, c_funloc(whatWindow), c_null_ptr)

      moreTestsMenu = uiNewMenu("More Tests"//c_null_char)
      quitEnabledItem = uiMenuAppendCheckItem(moreTestsMenu, "Quit Item Enabled"//c_null_char)
      call uiMenuItemSetChecked(quitEnabledItem, 1)
      prefsEnabledItem = uiMenuAppendCheckItem(moreTestsMenu, "Preferences Item Enabled"//c_null_char)
      call uiMenuItemSetChecked(prefsEnabledItem, 1)
      aboutEnabledItem = uiMenuAppendCheckItem(moreTestsMenu, "About Item Enabled"//c_null_char)
      call uiMenuItemSetChecked(aboutEnabledItem, 1)
      call uiMenuAppendSeparator(moreTestsMenu)
      checkEnabledItem = uiMenuAppendCheckItem(moreTestsMenu, "Check Me Item Enabled"//c_null_char)
      call uiMenuItemSetChecked(checkEnabledItem, 1)

      multiMenu = uiNewMenu("Multi"//c_null_char)
      call uiMenuAppendSeparator(multiMenu)
      call uiMenuAppendSeparator(multiMenu)
      dummy = uiMenuAppendItem(multiMenu, "Item && Item && Item"//c_null_char)
      call uiMenuAppendSeparator(multiMenu)
      call uiMenuAppendSeparator(multiMenu)
      dummy = uiMenuAppendItem(multiMenu, "Item __ Item __ Item"//c_null_char)
      call uiMenuAppendSeparator(multiMenu)
      call uiMenuAppendSeparator(multiMenu)

      helpMenu = uiNewMenu("Help"//c_null_char)
      helpItem = uiMenuAppendItem(helpMenu, "Help"//c_null_char)
      aboutItem = uiMenuAppendAboutItem(helpMenu)

      call uiMenuItemOnClicked(quitEnabledItem, c_funloc(enableItemTest), quitItem)
      call uiMenuItemOnClicked(prefsEnabledItem, c_funloc(enableItemTest), prefsItem)
      call uiMenuItemOnClicked(aboutEnabledItem, c_funloc(enableItemTest), aboutItem)
      call uiMenuItemOnClicked(checkEnabledItem, c_funloc(enableItemTest), checkItem)

      call uiMenuItemOnClicked(aboutItem, c_funloc(onMsgBoxClicked), c_null_ptr)

  end subroutine initMenus

end module handlers

program main
    use iso_c_binding
    use libuif
    use handlers

    implicit none
!    type(c_ptr) :: mainwin
    type(c_ptr) :: vbox,ebtn,grid,button

! initialize
  call uiInit(0)

! start procedure

  call initMenus()

  mainwin = uiNewWindow("libui menu"//c_null_char, 400, 100, 1)
  call uiWindowSetMargined(mainwin, 1)
  call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)

  call uiOnShouldQuit(c_funloc(onShouldQuit), mainwin)

  vbox = uiNewVerticalBox()
  call uiWindowSetChild(mainwin, vbox)

  grid = uiNewGrid()
  call uiGridSetPadded(grid, 1)
  call uiBoxAppend(vbox, grid, 0)

  button = uiNewButton("Open File"//c_null_char)
  entry1 = uiNewEntry()
  call uiEntrySetReadOnly(entry1, 1)
  call uiButtonOnClicked(button, c_funloc(onOpenFileClicked), c_null_ptr)
  call uiGridAppend(grid, button,0, 0, 1, 1,0, uiAlignFill, 0, uiAlignFill)
  call uiGridAppend(grid, entry1,1, 0, 1, 1,1, uiAlignFill, 0, uiAlignFill)

  button = uiNewButton("Save File"//c_null_char)
  entry2 = uiNewEntry()
  call uiEntrySetReadOnly(entry2, 1)
  call uiButtonOnClicked(button, c_funloc(onSaveFileClicked), c_null_ptr)
  call uiGridAppend(grid, button,0, 1, 1, 1,0, uiAlignFill, 0, uiAlignFill)
  call uiGridAppend(grid, entry2,1, 1, 1, 1,1, uiAlignFill, 0, uiAlignFill)

  call uiBoxAppend(vbox,uiNewHorizontalSeparator(),0)

  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)

  call uiBoxAppend(vbox, ebtn, 0)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main
