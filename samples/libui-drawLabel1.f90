!
! libui-drawLabel1.f90
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

    type(c_ptr) :: drawLabel
    type(uiAreaHandler) :: drawLabelHandler

    character(200) :: drawText

!// and some colors
!// names and values from https://msdn.microsoft.com/en-us/library/windows/desktop/dd370907%28v=vs.85%29.aspx
    integer,parameter :: colorWhite = x'FFFFFF'
    integer,parameter :: colorBlack = x'000000'
    integer,parameter :: colorRed = x'FF0000'
    integer,parameter :: colorDodgerBlue = x'1E90FF'
    integer,parameter :: d2dBlack = x'000000'
    integer,parameter :: d2dLightSlateGray = x'778899'
    integer,parameter :: d2dCornflowerBlue = x'6495ED'
    integer,parameter :: d2dWhite = x'FFFFFF'
    integer,parameter :: d2dYellowGreen = x'9ACD32'
    integer,parameter :: d2dYellow = x'FFFF00'
    integer,parameter :: d2dForestGreen = x'228B22'
    integer,parameter :: d2dOliveDrab = x'6B8E23'
    integer,parameter :: d2dLightSkyBlue = x'87CEFA'

contains

! "close mainwindow" is a this app
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
      type(c_ptr) :: path
      type(uiDrawBrush) :: brush
      type(uiAreaDrawParams) :: dp
      type(c_ptr) :: layout
      type(c_ptr) :: textFont
      type(uiDrawTextFontDescriptor),pointer :: font
!      double precision :: r, g, b, al
      double precision :: width, height
      character(100,c_char),pointer :: family(:)
      allocate(family(1))
      allocate(font)

!  fill the area with white
!       call d2dSolidBrush(brush, d2dForestGreen, 1.0d0)
       call d2dSolidBrush(brush, colorWhite, 1.0d0)
!       path = uiDrawNewPath(uiDrawFillModeWinding)
       path = uiDrawNewPath(uiDrawFillModeAlternate)
       call uiDrawPathAddRectangle(path, 0.0_c_double, 0.0_c_double, dp%AreaWidth, dp%AreaHeight)
       call uiDrawPathEnd(path)
       call uiDrawFill(dp%Context, path, brush)
       call uiDrawFreePath(path)

! dummy draw with color in area and set foreground color 
!       call d2dSolidBrush(brush, colorBlack, 1.0d0)
       call d2dSolidBrush(brush, colorRed, 1.0d0)
       path = uiDrawNewPath(uiDrawFillModeAlternate)
       call uiDrawPathAddRectangle(path, 0.0_c_double, 0.0_c_double, 0.0d0,0.0d0)
       call uiDrawPathEnd(path)
       call uiDrawFill(dp%Context, path, brush)
       call uiDrawFreePath(path)

! draw text
!      family(1) = "Monospace"//nilc
      family(1) = "Arial"//nilc
      font%Family = c_loc(family(1))
      font%Size = 24.0d0
      font%Weight = uiDrawTextWeightBold
      font%Italic = uiDrawTextItalicItalic
      font%Stretch =uiDrawTextStretchUltraExpanded

      textFont = uiDrawLoadClosestFont(c_loc(font)) 
!      al = 1.0d0

      layout = uiDrawNewTextLayout(drawText, textFont, -1.0d0) 
!      layout = uiDrawNewTextLayout("日本語は表示出来るか？" //char(13) // "出来たね " //char(13) & 
!&      // "One two three four"//c_null_char, &
!&       textFont, -1.0d0)
      call uiDrawFreeTextFont(textFont)

! This if want set color for each character 
!      call d2dColorToRGB(colorBlack,r,g,b)
!      call uiDrawTextLayoutSetColor(layout, 0, 20, r,g,b, al)

      call uiDrawText(dp%Context, 10.0d0, 10.0d0, layout)
      call uiDrawTextLayoutExtents(layout, width, height)
      call uiDrawFreeTextLayout(layout)

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

end module handlers

program main
    use iso_c_binding
    use libuif_structs
    use libuif
    use handlers

    implicit none
!    type(c_ptr) :: mainwin
    type(c_ptr) :: vbox,ebtn,lbl1
!    type(c_ptr) :: drawLabel
!    type(uiAreaHandler) :: drawLabelHandler

! initialize
    call uiInit(0)

! start procedure

    mainwin = uiNewWindow("libui Draw Text"//c_null_char, 640, 480, 0)
    call uiWindowSetMargined(mainwin, 1)
    call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)

    vbox = uiNewVerticalBox()
    call uiWindowSetChild(mainwin, vbox)

    lbl1 = uiNewLabel("下はドローラベルです。"//nilc)
    call uiBoxAppend(vbox, lbl1, 1)

    drawLabelHandler%Draw = c_funloc(handlerDraw)
    drawLabelHandler%MouseEvent = c_funloc(handlerMouseEvent)
    drawLabelHandler%MouseCrossed = c_funloc(handlerMouseCrossed)
    drawLabelHandler%DragBroken = c_funloc(handlerDragBroken)
    drawLabelHandler%KeyEvent = c_funloc(handlerKeyEvent)
    drawLabel = uiNewArea(drawLabelHandler)
    call uiBoxAppend(vbox, drawLabel, 1)

    drawText = "表示テキストのパラメータ化"//nilc

!  call uiBoxSetPadded(vbox,1)
  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)
  call uiBoxAppend(vbox, ebtn, 0)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main
