!
! drawLabel_center.f90
! Fortran から　libui　を使う。
!
! Author Yoji.Hosokawa 2016/4
!
! gfortran -O2 -g -Wall -o ./drawLabel_center drawLabel_center.f90 -L. -lui -llibuif_parts `pkg-config gtk+-3.0 --libs` -ldl -lpthread
!

module handlers
  use iso_c_binding
  use libuif_structs
  use libuif
  implicit none

    type(c_ptr) :: mainwin;

    type(c_ptr) :: drawLabel,entry1
    type(uiAreaHandler) :: drawLabelHandler

    character(200),pointer :: drawText

    integer :: labelUpFlag = 0
    
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

  subroutine drawLabelup()
    use iso_c_binding
    use libuif
    use libuif_parts
    type(c_ptr) :: text_c
      text_c = uiEntryText(entry1)
      call c_f_pointer(text_c,drawText)
      labelUpFlag = 1
      call uiAreaQueueRedrawAll(drawLabel)
  end subroutine drawLabelup

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
    use libuif_parts
      implicit none
      type(uiAreaHandler) :: a
      type(c_ptr) :: area
      type(c_ptr) :: path
      type(uiDrawBrush) :: brush
      type(uiAreaDrawParams) :: dp
      type(c_ptr) :: layout
      type(c_ptr) :: textFont

      double precision :: cx,cy,fontSize,strlenw
!      double precision :: r, g, b, al
!      double precision :: width, height
      type(uiDrawTextFontDescriptor),pointer :: font
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
       call d2dSolidBrush(brush, colorBlack, 1.0d0)
!       call d2dSolidBrush(brush, colorRed, 1.0d0)
       path = uiDrawNewPath(uiDrawFillModeAlternate)
!       call uiDrawPathAddRectangle(path, 0.0_c_double, 0.0_c_double, 0.0d0,0.0d0)
       call uiDrawPathEnd(path)
       call uiDrawFill(dp%Context, path, brush)
       call uiDrawFreePath(path)

! select font size & position
      fontSize = 20.0d0
      strlenw = utf8lenw(drawText)

      cx = dp%AreaWidth / 2.0d0 - fontSize * strlenw * 0.64d0 / 2.0d0
      if (cx < 0) then
        cx = 0.0d0
      end if
      cy = dp%AreaHeight / 2.0d0 - fontSize / 2.0d0 - fontSize * 0.15d0    

! set font
!      family(1) = "Monospace"//nilc
      family(1) = "Arial"//nilc
      font%Family = c_loc(family(1))
      font%Size = fontSize
      font%Weight = uiDrawTextWeightNormal
      font%Italic = uiDrawTextItalicNormal
      font%Stretch = uiDrawTextStretchNormal
      textFont = uiDrawLoadClosestFont(c_loc(font)) 
!      al = 1.0d0


! when drawLabelup
     if ( labelUpFlag == 1) then

!  fill the area with white
       call d2dSolidBrush(brush, d2dForestGreen, 1.0d0)
!       call d2dSolidBrush(brush, colorWhite, 1.0d0)
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
!       call uiDrawPathAddRectangle(path, 0.0_c_double, 0.0_c_double, 0.0d0,0.0d0)
       call uiDrawPathEnd(path)
       call uiDrawFill(dp%Context, path, brush)
       call uiDrawFreePath(path)

! select font size & position
      fontSize = 28.0d0
      strlenw = utf8lenw(drawText)

      cx = dp%AreaWidth / 2.0d0 - fontSize * strlenw * 0.64d0 / 2.0d0
      if (cx < 0) then
        cx = 0.0d0
      end if
      cy = dp%AreaHeight / 2.0d0 - fontSize / 2.0d0 - fontSize * 0.15d0    

! set font
!      family(1) = "Monospace"//nilc
      family(1) = "Arial"//nilc
      font%Family = c_loc(family(1))
      font%Size = fontSize
      font%Weight = uiDrawTextWeightNormal
      font%Italic = uiDrawTextItalicNormal
      font%Stretch = uiDrawTextStretchNormal
      textFont = uiDrawLoadClosestFont(c_loc(font)) 

     endif

! draw text

      layout = uiDrawNewTextLayout(drawText, textFont, -1.0d0)
      call uiDrawFreeTextFont(textFont)

! This if want set color for each character 
!      call d2dColorToRGB(colorBlack,r,g,b)
!      call uiDrawTextLayoutSetColor(layout, 0, 20, r,g,b, al)

      call uiDrawText(dp%Context, cx, cy , layout)
!      call uiDrawText(dp%Context, 10.0d0, 10.0d0, layout)
!      call uiDrawTextLayoutExtents(layout, width, height)
      call uiDrawFreeTextLayout(layout)

  end subroutine handlerDraw

  subroutine handlerMouseEvent()
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
    type(c_ptr) :: vbox,btn1,ebtn
    type(c_ptr) :: msggrid,hbox

! initialize
    call uiInit(0)

! start procedure

    mainwin = uiNewWindow("drawLabel centering Test"//c_null_char, 640, 480, 0)
    call uiWindowSetMargined(mainwin, 1)
    call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)

    vbox = uiNewVerticalBox()
    call uiWindowSetChild(mainwin, vbox)

    hbox = uiNewHorizontalBox()
    call uiBoxAppend(vbox, hbox, 1)

    msggrid = uiNewGrid()
    call uiGridSetPadded(msggrid, 1)
    call uiBoxAppend(hbox, msggrid, 1)

    entry1 = uiNewEntry()
    call uiGridAppend(msggrid, entry1,0,0,1,1,0, uiAlignFill, 0, uiAlignFill)

    drawLabelHandler%Draw = c_funloc(handlerDraw)
    drawLabelHandler%MouseEvent = c_funloc(handlerMouseEvent)
    drawLabelHandler%MouseCrossed = c_funloc(handlerMouseCrossed)
    drawLabelHandler%DragBroken = c_funloc(handlerDragBroken)
    drawLabelHandler%KeyEvent = c_funloc(handlerKeyEvent)
    drawLabel = uiNewArea(drawLabelHandler)
    call uiGridAppend(msggrid, drawLabel,0,1,2,1,1, uiAlignFill, 1, uiAlignFill)

    allocate(drawText)
    drawText = "真ん中に表示するかな？？？"//nilc

! drawLabel update callback
    btn1 = uiNewButton("センタリング表示"//c_null_char)
    call uiButtonOnClicked(btn1, c_funloc(drawLabelup), c_null_ptr)
    call uiGridAppend(msggrid, btn1,0,3,1,1,1, uiAlignFill, 0, uiAlignFill)

!    call uiBoxSetPadded(vbox,1)
    ebtn = uiNewButton("終了"//c_null_char)
    call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)
    call uiGridAppend(msggrid, ebtn,1,3,1,1,0, uiAlignFill, 0, uiAlignFill)

    call uiControlShow(mainwin)
    call uiMain()

end program main
