!
! libui-draw-text.f90
! Fortran から　libui　を使う。
!
! Author Yoji.Hosokawa 2016/4
!
! gfortran -O2 -g -Wall -o libui-window libui-window.f90 -L.-lui `pkg-config gtk+-3.0 --libs`
!

module handlers
  use iso_c_binding
  use libuif_structs
  use libuif
  implicit none

    type(c_ptr) :: mainwin;

    type(c_ptr) :: textString
    type(c_ptr) :: textFontButton
    type(c_ptr) :: textColorButton
    type(c_ptr) :: textWidth
    type(c_ptr) :: textApply
    type(c_ptr) :: noZ
    type(c_ptr) :: textArea
    type(uiAreaHandler) :: textAreaHandler

    integer,parameter :: colorWhite = x'FFFFFF'
    integer,parameter :: colorBlack = x'000000'

contains

  ! "close mainwindow" is a this app

  function shouldQuit() result(ret)
    use iso_c_binding
    use libuif
      integer :: ret
        call uiControlDestroy(mainwin)
        ret = 1
  end function shouldQuit

  subroutine onClosing()
    use iso_c_binding
    use libuif
      call uiQuit()
  end subroutine onClosing

  subroutine d2dColorToRGB(color, r, g, b)
    use iso_c_binding
    use libuif
      integer :: color
      double precision :: r,g,b
        r = dble(ibits(color,16,8)) / 255.0d0
        g = dble(ibits(color,8,8)) / 255.0d0
        b = dble(ibits(color,0,8)) / 255.0d0
  end subroutine d2dColorToRGB

  subroutine d2dSolidBrush(brush, color, alpha)
    use iso_c_binding
    use libuif_structs
    use libuif
      type(uiDrawBrush) :: brush
      integer :: color
      double precision :: alpha
        brush%Type = uiDrawBrushTypeSolid
        call d2dColorToRGB(color, brush%R, brush%G, brush%B)
        brush%A = alpha;
  end subroutine d2dSolidBrush

  subroutine handlerDraw(a, area, dp)
    use iso_c_binding
    use libuif_structs
    use libuif
    implicit none
      type(uiAreaHandler) :: a
      type(c_ptr) :: area
      type(uiAreaDrawParams) :: dp
      type(c_ptr) :: font
      type(c_ptr) :: layout
      double precision :: r, g, b, al
      character :: surrogates(7), composed(12)

      double precision :: width, height

! draw text
       font = uiFontButtonFont(textFontButton)

      layout = uiDrawNewTextLayout("One two three four"//c_null_char, font, -1.0d0)
!      call uiDrawTextLayoutSetColor(layout, 0, 3, 0.0d0, 0.0d0, 0.0d0, 1.0d0)
      call uiDrawTextLayoutSetColor(layout, 4, 7, 1.0d0, 0.0d0, 0.0d0, 1.0d0)
      call uiDrawTextLayoutSetColor(layout, 8, 14,1.0d0, 0.0d0, 0.5d0, 0.5d0)
      call uiColorButtonColor(textColorButton, r, g, b, al)
      call uiDrawTextLayoutSetColor(layout, 14, 18, r, g, b, al)
      call uiDrawText(dp%Context, 10.0d0, 10.0d0, layout)
      call uiDrawTextLayoutExtents(layout, width, height)
      call uiDrawFreeTextLayout(layout)

      surrogates(1) = char(120)         ! x
      surrogates(2) = char(x'F0')       !// surrogates D800 DF08
      surrogates(3) = char(x'90')
      surrogates(4) = char(x'8C')
      surrogates(5) = char(x'88')
      surrogates(6) = char(121)         !y
      surrogates(7) = char(0)

      layout = uiDrawNewTextLayout(surrogates, font, -1.0d0)
!      call uiDrawTextLayoutSetColor(layout, 0, 6, 0.0d0, 0.0d0, 0.0d0, 1.0d0)
      call uiDrawTextLayoutSetColor(layout, 1, 2, 1.0d0, 0.0d0, 0.5d0, 0.5d0)
      call uiDrawText(dp%Context, 10.0d0, 10.0d0 + height, layout)
      call uiDrawFreeTextLayout(layout)

      composed(1) = char(122)       ! z
      composed(2) = char(122)       ! z
      composed(3) = char(x'C3')     !// 2
      composed(4) = char(x'A9')
      composed(5) = char(122)       ! z
      composed(6) = char(122)       ! z
      composed(7) = char(x'65')     !// 5
      composed(8) = char(x'CC')
      composed(9) = char(x'81')
      composed(10) = char(122)       ! z
      composed(11) = char(122)       ! z
      composed(12) = char(0)

      layout = uiDrawNewTextLayout(composed, font, -1.0d0)
!      call uiDrawTextLayoutSetColor(layout, 0, 11, 0.0d0, 0.0d0, 0.0d0, 1.0d0)
      call uiDrawTextLayoutSetColor(layout, 2, 3, 1.0d0, 0.0d0, 0.5d0, 0.5d0)
      call uiDrawTextLayoutSetColor(layout, 5, 6, 1.0d0, 0.0d0, 0.5d0, 0.5d0)
      if (uiCheckboxChecked(noZ) == 0) then
        call uiDrawTextLayoutSetColor(layout, 6, 7, 0.5d0, 0.0d0, 1.0d0, 0.5d0)
      end if
      call uiDrawText(dp%Context, 10.0d0, 10.0d0 + height + height, layout)
      call uiDrawFreeTextLayout(layout)

      call uiDrawFreeTextFont(font)

  end subroutine handlerDraw

  subroutine handlerMouseEvent()
    use iso_c_binding
    use libuif
!  do nothing
  end subroutine handlerMouseEvent

  subroutine handlerMouseCrossed() 
    use iso_c_binding
    use libuif
!  do nothing
  end subroutine handlerMouseCrossed 

  subroutine handlerDragBroken()
    use iso_c_binding
    use libuif
!  do nothing
  end subroutine handlerDragBroken

  function handlerKeyEvent() result(ret)
    use iso_c_binding
    use libuif
      integer :: ret
!  reject all keys
    ret = 0
  end function handlerKeyEvent

  subroutine onFontChanged()
    use iso_c_binding
    use libuif
      call uiAreaQueueRedrawAll(textArea)
  end subroutine onFontChanged

  subroutine onColorChanged()
    use iso_c_binding
    use libuif
    use iso_c_binding
    use libuif
      call uiAreaQueueRedrawAll(textArea)
  end subroutine onColorChanged

  subroutine onNoZ()
    use iso_c_binding
    use libuif
      call uiAreaQueueRedrawAll(textArea)
  end subroutine onNoZ

end module handlers

program main
    use iso_c_binding
    use libuif_structs
    use libuif
    use handlers

    implicit none
!    type(c_ptr) :: mainwin
    type(c_ptr) :: vbox,hbox,ebtn
    type(c_ptr) :: mainTab

! initialize
    call uiInit(0)
  
    call uiOnShouldQuit(c_funloc(shouldQuit), c_null_ptr)

! start procedure

    mainwin = uiNewWindow("libui Draw Text"//c_null_char, 640, 480, 0)
!  call uiWindowSetMargined(mainwin, 1)
    call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)

    mainTab = uiNewTab()
    call uiWindowSetChild(mainwin, mainTab)

    vbox = uiNewVerticalBox()
    call uiTabAppend(mainTab, "Page 10", vbox)

    hbox = uiNewHorizontalBox()
    call uiBoxAppend(vbox, hbox, 0)

    textString = uiNewEntry()
!  // TODO make it placeholder
    call uiEntrySetText(textString, "Enter text here"//c_null_char)
    call uiBoxAppend(hbox, textString, 1)

    textFontButton = uiNewFontButton()
    call uiFontButtonOnChanged(textFontButton, c_funloc(onFontChanged), c_null_ptr)
    call uiBoxAppend(hbox, textFontButton, 1)

    textColorButton = uiNewColorButton()
    call uiColorButtonOnChanged(textColorButton, c_funloc(onColorChanged), c_null_ptr)
    call uiBoxAppend(hbox, textColorButton, 1)

    hbox = uiNewHorizontalBox()
    call uiBoxAppend(vbox, hbox, 0)

    textApply = uiNewButton("Apply"//c_null_char)
    call uiBoxAppend(hbox, textApply, 1)

    textWidth = uiNewEntry()
    call uiEntrySetText(textWidth, "-1"//c_null_char)
    call uiBoxAppend(hbox, textWidth, 1)

    noZ = uiNewCheckbox("No Z Color"//c_null_char)
    call uiCheckboxOnToggled(noZ, c_funloc(onNoZ), c_null_ptr)
    call uiBoxAppend(hbox, noZ, 0)

    textAreaHandler%Draw = c_funloc(handlerDraw)
    textAreaHandler%MouseEvent = c_funloc(handlerMouseEvent)
    textAreaHandler%MouseCrossed = c_funloc(handlerMouseCrossed)
    textAreaHandler%DragBroken = c_funloc(handlerDragBroken)
    textAreaHandler%KeyEvent = c_funloc(handlerKeyEvent)
    textArea = uiNewArea(textAreaHandler)
    call uiBoxAppend(vbox, textArea, 1)

!    // dummy objects to test single-activation
    hbox = uiNewHorizontalBox()
    call uiBoxAppend(vbox, hbox, 0)
    call uiBoxAppend(hbox, uiNewFontButton(), 1)
    call uiBoxAppend(hbox, uiNewColorButton(), 1)

!  call uiBoxSetPadded(vbox,1)
  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)
  call uiBoxAppend(vbox, ebtn, 0)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main
