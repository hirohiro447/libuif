!
! libui-draw-sample1.f90
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

!  subroutine graphSize(clientWidth, clientHeight)
!    use iso_c_binding
!    implicit none
!      double precision :: clientWidth, clientHeight
!        graphWidth = clientWidth - xoffLeft - xoffRight;
!        graphHeight = clientHeight - yoffTop - yoffBottom;
!  end subroutine graphSize

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

  subroutine drawD2DLinearBrush(p) 
    use iso_c_binding
    use libuif_structs
    use libuif
      type(uiAreaDrawParams) :: p
      type(c_ptr) :: path
      type(uiDrawBrush) :: black
      type(uiDrawBrush) :: gradient
      type(uiDrawStrokeParams) :: sp
      type(uiDrawMatrix) :: m

      type(uiDrawBrushGradientStop),pointer :: stops(:)
      allocate( stops(2)) 

      sp%Dashes = c_null_ptr
      sp%NumDashes = 0
      sp%DashPhase = 0.0d0

!   leave some room
      call uiDrawMatrixSetIdentity(m)
      call uiDrawMatrixTranslate(m, 25.0d0, 25.0d0)
      call uiDrawTransform(p%Context, m)

      gradient%Type = uiDrawBrushTypeLinearGradient
      gradient%X0 = 0 + 400
      gradient%Y0 = 0
      gradient%X1 = 150 + 400
      gradient%Y1 = 150
      stops(1)%Pos = 0.0
      call d2dColorToRGB(d2dYellow, stops(1)%R, stops(1)%G, stops(1)%B)
      stops(1)%A = 1.0
      stops(2)%Pos = 1.0
      call d2dColorToRGB(d2dForestGreen, stops(2)%R, stops(2)%G, stops(2)%B)
      stops(2)%A = 1.0
      gradient%Stops = c_loc(stops(1))
      gradient%NumStops = 2

      call d2dSolidBrush(black, d2dBlack, 1.0d0)

      path = uiDrawNewPath(uiDrawFillModeWinding)
      call uiDrawPathAddRectangle(path, 0.0d0 + 400.0d0, 0.0d0, 150.0d0, 150.0d0)
      call uiDrawPathEnd(path)

      call uiDrawFill(p%Context, path, gradient)
      sp%Thickness = 1.0
      sp%Cap = uiDrawLineCapFlat
      sp%Join = uiDrawLineJoinMiter
      sp%MiterLimit = uiDrawDefaultMiterLimit
      call uiDrawStroke(p%Context, path, black, sp)

      call uiDrawFreePath(path)
  end subroutine drawD2DLinearBrush

  subroutine drawD2DRadialBrush(p) 
    use iso_c_binding
    use libuif_structs
    use libuif
      type(uiAreaDrawParams) :: p
      type(c_ptr) :: path
      type(uiDrawBrush) :: black
      type(uiDrawBrush) :: gradient
      type(uiDrawStrokeParams) :: sp
      type(uiDrawMatrix) :: m

      type(uiDrawBrushGradientStop),pointer :: stops(:)
      allocate(stops(2)) 

      sp%Dashes = c_null_ptr
      sp%NumDashes = 0
      sp%DashPhase = 0.0d0

!   leave some room
      call uiDrawMatrixSetIdentity(m)
      call uiDrawMatrixTranslate(m, 25.0d0, 25.0d0)
      call uiDrawTransform(p%Context, m)

      gradient%Type = uiDrawBrushTypeRadialGradient
      gradient%X0 = 75
      gradient%Y0 = 75 + 200
      gradient%X1 = 75
      gradient%Y1 = 75 + 200
      gradient%OuterRadius = 75
      stops(1)%Pos = 0.0
      call d2dColorToRGB(d2dYellow, stops(1)%R, stops(1)%G, stops(1)%B)
      stops(1)%A = 1.0
      stops(2)%Pos = 1.0
      call d2dColorToRGB(d2dForestGreen, stops(2)%R, stops(2)%G, stops(2)%B)
      stops(2)%A = 1.0
      gradient%Stops = c_loc(stops(1))
      gradient%NumStops = 2

      call d2dSolidBrush(black, d2dBlack, 1.0d0)

      path = uiDrawNewPath(uiDrawFillModeWinding)
      call uiDrawPathNewFigure(path, 150.0d0, 75.0d0 + 200.0d0)
      call uiDrawPathArcTo(path, 75.0d0, 75.0d0 + 200.0d0,75.0d0,0.0d0,2.0d0 * uiPi,0)
      call uiDrawPathEnd(path)

      call uiDrawFill(p%Context, path, gradient)
      sp%Thickness = 1.0
      sp%Cap = uiDrawLineCapFlat
      sp%Join = uiDrawLineJoinMiter
      sp%MiterLimit = uiDrawDefaultMiterLimit
      call uiDrawStroke(p%Context, path, black, sp)

      call uiDrawFreePath(path)
  end subroutine drawD2DRadialBrush

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
!       sp%Cap = uiDrawLineCapFlat
!       sp%Join = uiDrawLineJoinMiter
!       sp%Thickness = 2.0d0
!       sp%MiterLimit = uiDrawDefaultMiterLimit

!  drawD2DLinearBrush
      call drawD2DLinearBrush(p)

!  drawD2DRadialBrush
      call drawD2DRadialBrush(p)
      
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

  mainwin = uiNewWindow("libui Draw Sample"//c_null_char, 640, 480, 0)
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
