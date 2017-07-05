!
! libui-draw-sample.f90
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
    type(c_ptr) :: histogram;
    type(uiAreaHandler) :: handler
    type(uiDrawBrush) :: brush
    type(uiDrawStrokeParams) :: sp
    type(uiDrawMatrix) :: m
    real(c_double) :: graphWidth, graphHeight
    real(c_double) :: graphR, graphG, graphB, graphA
    integer(c_int32_t) :: color
    real(c_double) :: alpha 

!    integer :: currentPoint = -1

!  some metrics
    integer,parameter :: xoffLeft = 20
    integer,parameter :: yoffTop = 20
    integer,parameter :: xoffRight = 20
    integer,parameter :: yoffBottom = 20
    integer,parameter :: pointRadius = 5

!// and some colors
!// names and values from https://msdn.microsoft.com/en-us/library/windows/desktop/dd370907%28v=vs.85%29.aspx
    integer,parameter :: colorWhite = x'FFFFFF'
    integer,parameter :: colorBlack = x'000000'
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

  subroutine setSolidBrush(linetype, color, alpha)
    use iso_c_binding
    use libuif
    implicit none
       integer :: linetype,color
       double precision :: alpha

         brush%Type = linetype
         brush%R = dble(ibits(color,16,8)) / 255.0d0
         brush%G = dble(ibits(color,8,8)) / 255.0d0
         brush%B = dble(ibits(color,0,8)) / 255.0d0
         brush%A = alpha
  end subroutine setSolidBrush

  subroutine graphSize(clientWidth, clientHeight)
    use iso_c_binding
    implicit none
      double precision :: clientWidth, clientHeight
        graphWidth = clientWidth - xoffLeft - xoffRight;
        graphHeight = clientHeight - yoffTop - yoffBottom;
  end subroutine graphSize

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

  subroutine d2dClear(p, color, alpha)
    use iso_c_binding
    use libuif_structs
    use libuif
      type(uiAreaDrawParams) :: p
      integer :: color
      double precision :: alpha
      type(c_ptr) :: path
      type(uiDrawBrush) :: brush

        call d2dSolidBrush(brush, color, alpha)
        path = uiDrawNewPath(uiDrawFillModeWinding)
        call uiDrawPathAddRectangle(path, 0.0d0, 0.0d0, p%AreaWidth, p%AreaHeight)
        call uiDrawPathEnd(path)
        call uiDrawFill(p%Context, path, brush)
        call uiDrawFreePath(path)
  end subroutine d2dClear

  subroutine drawD2DW8QS(p) 
    use iso_c_binding
    use libuif_structs
    use libuif
      type(uiAreaDrawParams) :: p
      type(c_ptr) :: path
      type(uiDrawBrush) :: brush

!        call setSolidBrush(uiDrawBrushTypeSolid, d2dForestGreen, 1.0d0)
        call d2dSolidBrush(brush, d2dForestGreen, 1.0d0)
        path = uiDrawNewPath(uiDrawFillModeWinding);
        call uiDrawPathAddRectangle(path,500.0d0,100.0d0,(p%AreaWidth - 300.0d0) - 300.0d0,(p%AreaHeight - 100.0d0) - 500.0d0)
        call uiDrawPathEnd(path)
        call uiDrawFill(p%Context, path, brush)
        call uiDrawFreePath(path)
  end subroutine drawD2DW8QS

  subroutine drawD2DSimpleApp(p) 
    use iso_c_binding
    use libuif_structs
    use libuif
      type(uiAreaDrawParams) :: p
      type(c_ptr) :: path
      type(uiDrawBrush) :: brush
      type(uiDrawBrush) :: lightSlateGray
      type(uiDrawBrush) :: cornflowerBlue
      type(uiDrawStrokeParams) :: sp
      integer :: x, y
      double precision :: left, top, right, bottom
        sp%Dashes = c_null_ptr
        sp%NumDashes = 0
        sp%DashPhase = 0.0d0

        call d2dSolidBrush(lightSlateGray, d2dLightSlateGray, 1.0d0)
        call d2dSolidBrush(cornflowerBlue, d2dCornflowerBlue, 1.0d0)

!        call d2dClear(p, d2dWhite, 1.0d0)

        sp%Thickness = 0.5d0
        sp%Cap = uiDrawLineCapFlat
        sp%Join = uiDrawLineJoinMiter
        sp%MiterLimit = uiDrawDefaultMiterLimit

        do x = 0, int(p%AreaWidth) + 1, 10
          path = uiDrawNewPath(uiDrawFillModeWinding)
          call uiDrawPathNewFigure(path, dble(x), 0.0d0)
          call uiDrawPathLineTo(path, dble(x), p%AreaHeight)
          call uiDrawPathEnd(path)
          call uiDrawStroke(p%Context, path, lightSlateGray, sp)
          call uiDrawFreePath(path)
        end do

        do y = 0, int(p%AreaHeight) + 1, 10
          path = uiDrawNewPath(uiDrawFillModeWinding)
          call uiDrawPathNewFigure(path, 0.0d0, dble(y))
          call uiDrawPathLineTo(path, p%AreaWidth, dble(y))
          call uiDrawPathEnd(path)
          call uiDrawStroke(p%Context, path, lightSlateGray, sp)
          call uiDrawFreePath(path)
        end do

        left = p%AreaWidth / 2.0d0 - 50.0d0
        right = p%AreaWidth / 2.0d0 + 50.0d0
        top = p%AreaHeight / 2.0d0 - 50.0d0
        bottom = p%AreaHeight / 2.0d0 + 50.0d0
        path = uiDrawNewPath(uiDrawFillModeWinding)
        call uiDrawPathAddRectangle(path, left, top, right - left, bottom - top)
        call uiDrawPathEnd(path)
        call uiDrawFill(p%Context, path, lightSlateGray)
        call uiDrawFreePath(path)

        left = p%AreaWidth / 2.0d0 - 100.0d0
        right = p%AreaWidth / 2.0d0 + 100.0d0
        top = p%AreaHeight / 2.0d0 - 100.0d0
        bottom = p%AreaHeight / 2.0d0 + 100.0d0
        path = uiDrawNewPath(uiDrawFillModeWinding)
        call uiDrawPathAddRectangle(path, left, top, right - left, bottom - top)
        call uiDrawPathEnd(path)
        sp%Thickness = 1.0d0
        call uiDrawStroke(p%Context, path, cornflowerBlue, sp)
        call uiDrawFreePath(path)

  end subroutine drawD2DSimpleApp

  subroutine drawD2DSolidBrush(p)
    use iso_c_binding
    use libuif_structs
    use libuif
      type(uiAreaDrawParams) :: p
      type(c_ptr)	:: path
      type(uiDrawBrush) black
      type(uiDrawBrush) yellowGreen
      type(uiDrawStrokeParams) sp

        sp%Dashes = c_null_ptr
        sp%NumDashes = 0
        sp%DashPhase = 0.0d0

        call d2dSolidBrush(black, d2dBlack, 1.0d0)
        call d2dSolidBrush(yellowGreen, d2dYellowGreen, 1.0d0)

        path = uiDrawNewPath(uiDrawFillModeWinding)
!	// the example doesn't define a rectangle
!	// 150x150 seems to be right given the other examples though
        call uiDrawPathAddRectangle(path, 25.0d0, 25.0d0, 150.0d0, 150.0d0)
        call uiDrawPathEnd(path)

        call uiDrawFill(p%Context, path, yellowGreen)
        sp%Thickness = 1.0d0
        sp%Cap = uiDrawLineCapFlat
        sp%Join = uiDrawLineJoinMiter
        sp%MiterLimit = uiDrawDefaultMiterLimit
        call uiDrawStroke(p%Context, path, black, sp)

        call uiDrawFreePath(path)
  end subroutine drawD2DSolidBrush

  subroutine handlerDraw(a, area, p)
    use iso_c_binding
    use libuif_structs
    use libuif
    implicit none
      type(uiAreaHandler) :: a
      type(c_ptr) :: area
      type(uiAreaDrawParams) :: p
      type(c_ptr) :: path
!  fill the area with white
       call setSolidBrush(uiDrawBrushTypeSolid, colorWhite, 1.0d0)
       path = uiDrawNewPath(uiDrawFillModeWinding)
       call uiDrawPathAddRectangle(path, 0.0_c_double, 0.0_c_double, p%AreaWidth, p%AreaHeight)
       call uiDrawPathEnd(path)
       call uiDrawFill(p%Context, path, brush)
       call uiDrawFreePath(path)

!  figure out dimensions
!       call graphSize(p%AreaWidth, p%AreaHeight)

!   make a stroke for both the axes and the histogram line
       sp%Cap = uiDrawLineCapFlat
       sp%Join = uiDrawLineJoinMiter
       sp%Thickness = 2.0d0
       sp%MiterLimit = uiDrawDefaultMiterLimit

!   draw the line
       call setSolidBrush(uiDrawBrushTypeSolid, colorBlack, 1.0d0)
       path = uiDrawNewPath(uiDrawFillModeWinding)
       call uiDrawPathNewFigure(path,dble(xoffLeft), dble(yoffTop) + 500.0d0)
       call uiDrawPathLineTo(path, dble(xoffLeft) + 30.0d0, dble(yoffTop) + 40.0d0 + 500.0d0)
       call uiDrawPathLineTo(path, dble(xoffLeft) + 330.0d0, dble(yoffTop) + 100.0d0 + 500.0d0)
       call uiDrawPathEnd(path)
       call uiDrawStroke(p%Context, path, brush, sp)
       call uiDrawFreePath(path)

!   draw the arc
      sp%Dashes = c_null_ptr
      sp%NumDashes = 0
      sp%DashPhase = 0.00d0
      call setSolidBrush(uiDrawBrushTypeSolid, colorDodgerBlue, 1.0d0)
      path = uiDrawNewPath(uiDrawFillModeWinding)
      call  uiDrawPathNewFigureWithArc(path,200.0d0,250.0d0,60.0d0,0.0d0,uiPi*1.0d0,0)
      call  uiDrawPathEnd(path)
      call uiDrawStroke(p%Context, path, brush, sp)
!     call  uiDrawFill(p%Context, path, brush)
      call  uiDrawFreePath(path)

! drawD2DW8QS
      call drawD2DW8QS(p)
! drawD2DSimpleApp
      call drawD2DSimpleApp(p)
!  drawD2DSolidBrush
      call drawD2DSolidBrush(p)

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
    type(c_ptr) :: vbox,ebtn


! initialize
  call uiInit(0)

! start procedure

  mainwin = uiNewWindow("libui Draw Sample"//c_null_char, 800, 700, 0)
!  call uiWindowSetMargined(mainwin, 1)
  call uiWindowOnClosing(mainwin, c_funloc(onClosing), c_null_ptr)

  vbox = uiNewVerticalBox()
  call uiWindowSetChild(mainwin, vbox)

    handler%Draw = c_funloc(handlerDraw)
    handler%MouseEvent = c_funloc(handlerMouseEvent)
    handler%MouseCrossed = c_funloc(handlerMouseCrossed)
    handler%DragBroken = c_funloc(handlerDragBroken)
    handler%KeyEvent = c_funloc(handlerKeyEvent)
  histogram = uiNewArea(handler)
  call uiBoxAppend(vbox, histogram, 1)

!  call uiBoxSetPadded(vbox,1)
  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)
  call uiBoxAppend(vbox, ebtn, 0)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main
