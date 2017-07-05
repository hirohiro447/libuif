!
! drawButton.f90
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

    type(c_ptr) :: drawLabel,drawButton
    type(uiAreaHandler) :: drawLabelHandler,drawButtonHandler

    character(200) :: drawLabelText,drawButtonText
    integer :: onButton = 0, onClick = 0

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
      drawLabelText = "押されました！！"//nilc
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
      type(uiDrawTextFontDescriptor),pointer :: font
      character(100,c_char),pointer :: family(:)
      allocate(family(1))
      allocate(font)

!  fill the area with white
       call d2dSolidBrush(brush, colorWhite, 1.0d0)
       path = uiDrawNewPath(uiDrawFillModeWinding)
       call uiDrawPathAddRectangle(path, 0.0_c_double, 0.0_c_double, dp%AreaWidth, dp%AreaHeight)
       call uiDrawPathEnd(path)
       call uiDrawFill(dp%Context, path, brush)
       call uiDrawFreePath(path)

! dummy draw with color in area and set foreground color 
       call d2dSolidBrush(brush, colorBlack, 1.0d0)
       path = uiDrawNewPath(uiDrawFillModeAlternate)
       call uiDrawPathEnd(path)
       call uiDrawFill(dp%Context, path, brush)
       call uiDrawFreePath(path)

! select font size & position
      fontSize = 20.0d0
      strlenw = utf8lenw(drawLabelText)

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

! draw text

      layout = uiDrawNewTextLayout(drawLabelText, textFont, -1.0d0)
      call uiDrawFreeTextFont(textFont)

      call uiDrawText(dp%Context, cx, cy , layout)
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

  subroutine handlerDrawButton(a, area, dp)
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
      type(uiDrawTextFontDescriptor),pointer :: font
      character(100,c_char),pointer :: family(:)
      allocate(family(1))
      allocate(font)

!  fill the area with d2dLightSlateGray
       if ( onButton == 1) then
         call d2dSolidBrush(brush, d2dLightSlateGray, 0.3d0)
         if (onClick == 1) then
           call d2dSolidBrush(brush, d2dLightSlateGray, 0.7d0)
         else
           call d2dSolidBrush(brush, d2dLightSlateGray, 0.3d0)
         endif
       else
         call d2dSolidBrush(brush, d2dLightSlateGray, 0.1d0)
       endif       

       path = uiDrawNewPath(uiDrawFillModeWinding)
       call uiDrawPathAddRectangle(path, 0.0_c_double, 0.0_c_double, dp%AreaWidth, dp%AreaHeight)
       call uiDrawPathEnd(path)
       call uiDrawFill(dp%Context, path, brush)
       call uiDrawFreePath(path)

! dummy draw with color in area and set foreground color 
       call d2dSolidBrush(brush, colorBlack, 1.0d0)
       path = uiDrawNewPath(uiDrawFillModeAlternate)
       call uiDrawPathEnd(path)
       call uiDrawFill(dp%Context, path, brush)
       call uiDrawFreePath(path)

! select font size & position
      fontSize = 20.0d0
      strlenw = utf8lenw(drawButtonText)

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

! draw text

      layout = uiDrawNewTextLayout(drawButtonText, textFont, -1.0d0)
      call uiDrawFreeTextFont(textFont)

      call uiDrawText(dp%Context, cx, cy , layout)
      call uiDrawFreeTextLayout(layout)

  end subroutine handlerDrawButton

  subroutine drawButtonMouseEvent(a, area, e)
    use iso_c_binding
    use libuif_structs
    use libuif
    implicit none
      type(uiAreaHandler) :: a
      type(c_ptr) :: area
      type(uiAreaMouseEvent) :: e

      if (e%X >= 5.0d0 .and. e%X <= e%AreaWidth - 5.0d0 .and. e%Y >= 5.0d0 .and. e%Y <= e%AreaHeight - 5.0d0) then
        onButton = 1
        call uiAreaQueueRedrawAll(drawButton)
      else 
        onButton = 0
        call uiAreaQueueRedrawAll(drawButton)
      endif
     
! when mouse clicked      
! left: 1, center: 2, right: 3
      if ( e%Down == 1) then
        onClick = 1
        call uiAreaQueueRedrawAll(drawButton)
        call drawLabelup()
      else
        onClick = 0
        call uiAreaQueueRedrawAll(drawButton)
      end if

  end subroutine drawButtonMouseEvent

  subroutine drawButtonMouseCrossed() 
    use iso_c_binding
    use libuif
!  do nothing
  end subroutine drawButtonMouseCrossed 

  subroutine drawButtonDragBroken()
    use iso_c_binding
    use libuif
!  do nothing
  end subroutine drawButtonDragBroken

  function drawButtonKeyEvent() result(ret)
    use iso_c_binding
    use libuif
      integer :: ret
!  reject all keys
    ret = 0
  end function drawButtonKeyEvent

end module handlers

program main
    use iso_c_binding
    use libuif_structs
    use libuif
    use handlers

    implicit none
    type(c_ptr) :: mainwin
    type(c_ptr) :: vbox,ebtn
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

! drawLabel
    drawLabelHandler%Draw = c_funloc(handlerDraw)
    drawLabelHandler%MouseEvent = c_funloc(handlerMouseEvent)
    drawLabelHandler%MouseCrossed = c_funloc(handlerMouseCrossed)
    drawLabelHandler%DragBroken = c_funloc(handlerDragBroken)
    drawLabelHandler%KeyEvent = c_funloc(handlerKeyEvent)
    drawLabel = uiNewArea(drawLabelHandler)
    call uiGridAppend(msggrid, drawLabel,0,0,2,1,1, uiAlignFill, 1, uiAlignFill)
    drawLabelText = "真ん中に表示するかな？？？"//nilc

! drawButton
    drawButtonHandler%Draw = c_funloc(handlerDrawButton)
    drawButtonHandler%MouseEvent = c_funloc(drawButtonMouseEvent)
    drawButtonHandler%MouseCrossed = c_funloc(drawButtonMouseCrossed)
    drawButtonHandler%DragBroken = c_funloc(drawButtonDragBroken)
    drawButtonHandler%KeyEvent = c_funloc(drawButtonKeyEvent)
    drawButton = uiNewArea(drawButtonHandler)
    call uiGridAppend(msggrid, drawButton,0,2,2,1,1, uiAlignFill, 1, uiAlignFill)
    drawButtonText = "ドローボタンです。イベントは使えるか？"//nilc

!    call uiBoxSetPadded(vbox,1)
    ebtn = uiNewButton("終了"//c_null_char)
    call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)
    call uiGridAppend(msggrid, ebtn,1,4,1,1,1, uiAlignFill, 0, uiAlignFill)

    call uiControlShow(mainwin)
    call uiMain()

end program main
