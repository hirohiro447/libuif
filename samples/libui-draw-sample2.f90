!
! libui-draw-sample2.f90
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

  subroutine drawD2DPathGeometries(p) 
    use iso_c_binding
    use libuif_structs
    use libuif
      type(uiAreaDrawParams) :: p
      type(c_ptr) :: leftMountain
      type(c_ptr) :: rightMountain
      type(c_ptr) :: sun
      type(c_ptr) :: sunRays
      type(c_ptr) :: river
      type(uiDrawBrush) :: radial
      type(uiDrawBrush) :: scene
      type(uiDrawStrokeParams) :: sp
      type(uiDrawBrushGradientStop),pointer :: stops(:)
      allocate(stops(2))

      sp%Dashes = c_null_ptr
      sp%NumDashes = 0
      sp%DashPhase = 0

!   TODO this is definitely wrong but the example doesn't have the right brush in it
      radial%Type = uiDrawBrushTypeRadialGradient
      radial%X0 = 75
      radial%Y0 = 75
      radial%X1 = 75
      radial%Y1 = 75
      radial%OuterRadius = 75;
      stops(1)%Pos = 0.0
      call d2dColorToRGB(d2dYellow, stops(1)%R, stops(1)%G, stops(1)%B)
      stops(1)%A = 1.0
      stops(2)%Pos = 1.0
      call d2dColorToRGB(d2dForestGreen, stops(2)%R, stops(2)%G, stops(2)%B)
      stops(2)%A = 1.0
      radial%Stops = c_loc(stops(1))
      radial%NumStops = 2

      leftMountain = uiDrawNewPath(uiDrawFillModeWinding)
      call uiDrawPathNewFigure(leftMountain, 346.0d0, 255.0d0)
      call uiDrawPathLineTo(leftMountain, 267.0d0, 177.0d0)
      call uiDrawPathLineTo(leftMountain, 236.0d0, 192.0d0)
      call uiDrawPathLineTo(leftMountain, 212.0d0, 160.0d0)
      call uiDrawPathLineTo(leftMountain, 156.0d0, 255.0d0)
      call uiDrawPathLineTo(leftMountain, 346.0d0, 255.0d0)
      call uiDrawPathCloseFigure(leftMountain)
      call uiDrawPathEnd(leftMountain)

      rightMountain = uiDrawNewPath(uiDrawFillModeWinding)
      call uiDrawPathNewFigure(rightMountain, 575.0d0, 263.0d0)
      call uiDrawPathLineTo(rightMountain, 481.0d0, 146.0d0)
      call uiDrawPathLineTo(rightMountain, 449.0d0, 181.0d0)
      call uiDrawPathLineTo(rightMountain, 433.0d0, 159.0d0)
      call uiDrawPathLineTo(rightMountain, 401.0d0, 214.0d0)
      call uiDrawPathLineTo(rightMountain, 381.0d0, 199.0d0)
      call uiDrawPathLineTo(rightMountain, 323.0d0, 263.0d0)
      call uiDrawPathLineTo(rightMountain, 575.0d0, 263.0d0)
      call uiDrawPathCloseFigure(rightMountain)
      call uiDrawPathEnd(rightMountain)

      sun = uiDrawNewPath(uiDrawFillModeWinding)
      call uiDrawPathNewFigureWithArc(sun,(440.0d0 - 270.0d0) / 2.0d0 + 270.0d0, 255.0d0,85.0d0,uiPi, uiPi,0)
      call uiDrawPathCloseFigure(sun)
      call uiDrawPathEnd(sun)

!   the original examples had these as hollow figures
!   we don't support them, so we'll have to stroke it separately
      sunRays = uiDrawNewPath(uiDrawFillModeWinding)
      call uiDrawPathNewFigure(sunRays, 299.0d0, 182.0d0)
      call uiDrawPathBezierTo(sunRays,299.0d0, 182.0d0,294.0d0, 176.0d0,285.0d0, 178.0d0)
      call uiDrawPathBezierTo(sunRays,276.0d0, 179.0d0,272.0d0, 173.0d0,272.0d0, 173.0d0)
      call uiDrawPathNewFigure(sunRays, 354.0d0, 156.0d0)
      call uiDrawPathBezierTo(sunRays,354.0d0, 156.0d0,358.0d0, 149.0d0,354.0d0, 142.0d0)
      call uiDrawPathBezierTo(sunRays,349.0d0, 134.0d0,354.0d0, 127.0d0,354.0d0, 127.0d0)
      call uiDrawPathNewFigure(sunRays, 322.0d0, 164.0d0)
      call uiDrawPathBezierTo(sunRays,322.0d0, 164.0d0,322.0d0, 156.0d0,314.0d0, 152.0d0)
      call uiDrawPathBezierTo(sunRays,306.0d0, 149.0d0,305.0d0, 141.0d0,305.0d0, 141.0d0)
      call uiDrawPathNewFigure(sunRays, 385.0d0, 164.0d0)
      call uiDrawPathBezierTo(sunRays,385.0d0, 164.0d0,392.0d0, 161.0d0,394.0d0, 152.0d0)
      call uiDrawPathBezierTo(sunRays,395.0d0, 144.0d0,402.0d0, 141.0d0,402.0d0, 142.0d0)
      call uiDrawPathNewFigure(sunRays, 408.0d0, 182.0d0)
      call uiDrawPathBezierTo(sunRays,408.0d0, 182.0d0,416.0d0, 184.0d0,422.0d0, 178.0d0)
      call uiDrawPathBezierTo(sunRays,428.0d0, 171.0d0,435.0d0, 173.0d0,435.0d0, 173.0d0)
      call uiDrawPathEnd(sunRays)

      river = uiDrawNewPath(uiDrawFillModeWinding)
      call uiDrawPathNewFigure(river, 183.0d0, 392.0d0)
      call uiDrawPathBezierTo(river,238.0d0, 284.0d0,472.0d0, 345.0d0,356.0d0, 303.0d0)
      call uiDrawPathBezierTo(river,237.0d0, 261.0d0,333.0d0, 256.0d0,333.0d0, 256.0d0)
      call uiDrawPathBezierTo(river,335.0d0, 257.0d0,241.0d0, 261.0d0,411.0d0, 306.0d0)
      call uiDrawPathBezierTo(river,574.0d0, 350.0d0,288.0d0, 324.0d0,296.0d0, 392.0d0)
      call uiDrawPathEnd(river)

      call d2dClear(p, d2dWhite, 1.0d0)

!   TODO draw the grid

      call uiDrawFill(p%Context, sun, radial)

      call d2dSolidBrush(scene, d2dBlack, 1.0d0)
      sp%Thickness = 1.0
      sp%Cap = uiDrawLineCapFlat
      sp%Join = uiDrawLineJoinMiter
      sp%MiterLimit = uiDrawDefaultMiterLimit
      call uiDrawStroke(p%Context, sun, scene, sp)
      call uiDrawStroke(p%Context, sunRays, scene, sp)

      call d2dSolidBrush(scene, d2dOliveDrab, 1.0d0)
      call uiDrawFill(p%Context, leftMountain, scene)

      call d2dSolidBrush(scene, d2dBlack, 1.0d0)
      call uiDrawStroke(p%Context, leftMountain, scene, sp)

      call d2dSolidBrush(scene, d2dLightSkyBlue, 1.0d0)
      call uiDrawFill(p%Context, river, scene)

      call d2dSolidBrush(scene, d2dBlack, 1.0d0)
      call uiDrawStroke(p%Context, river, scene, sp)

      call d2dSolidBrush(scene, d2dYellowGreen, 1.0d0)
      call uiDrawFill(p%Context, rightMountain, scene)

      call d2dSolidBrush(scene, d2dBlack, 1.0d0)
      call uiDrawStroke(p%Context, rightMountain, scene, sp)

      call uiDrawFreePath(leftMountain)
      call uiDrawFreePath(rightMountain)
      call uiDrawFreePath(sun)
      call uiDrawFreePath(sunRays)
      call uiDrawFreePath(river)
  end subroutine drawD2DPathGeometries

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

!  drawD2DPathGeometries
      call drawD2DPathGeometries(p)

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
