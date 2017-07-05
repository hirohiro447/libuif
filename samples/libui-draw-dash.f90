!
! libui-draw-dash.f90
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

  subroutine crsourcergba(brush, r, g, b, a)
    use iso_c_binding
    use libuif_structs
    use libuif
    implicit none
      type(uiDrawBrush):: brush
      integer :: r,g,b,a
        brush%Type = uiDrawBrushTypeSolid
        brush%R = r
        brush%G = g
        brush%B = b
        brush%A = a
   end subroutine crsourcergba

  subroutine drawCSDash(p)
    use iso_c_binding
    use libuif_structs
    use libuif
    implicit none
      type(uiAreaDrawParams) :: p
      integer(kind=4) :: ndash
      double precision :: offset
      type(uiDrawBrush) :: source
      type(uiDrawStrokeParams) :: sp
      type(c_ptr) :: path

      double precision,pointer :: dashes(:)
      allocate( dashes(4)) 

      dashes = (/ 50.0d0, 10.0d0, 10.0d0, 10.0d0 /)
      ndash  = sizeof (dashes)/sizeof(dashes(0))
      offset = -50.0d0

      call crsourcergba(source, 0, 0, 0, 1)
      sp%Cap = uiDrawLineCapFlat
      sp%Join = uiDrawLineJoinMiter
      sp%MiterLimit = uiDrawDefaultMiterLimit
      sp%Dashes = c_loc(dashes(1))
      sp%NumDashes = ndash
      sp%DashPhase = offset
      sp%Thickness = 10.0d0

      path = uiDrawNewPath(uiDrawFillModeWinding)
      call uiDrawPathNewFigure(path, 128.0d0, 25.6d0)
      call uiDrawPathLineTo(path, 230.4d0, 230.4d0)
      call uiDrawPathLineTo(path, 230.4d0 -102.4d0, 230.4d0 + 0.0d0)
      call uiDrawPathBezierTo(path,51.2d0, 230.4d0,51.2d0, 128.0d0,128.0d0, 128.0d0)
      call uiDrawPathEnd(path)

      call uiDrawStroke(p%Context, path, source, sp)
      call uiDrawFreePath(path)
  end subroutine drawCSDash

  subroutine handlerDraw(a, area, p)
    use iso_c_binding
    use libuif_structs
    use libuif
    implicit none
      type(uiAreaHandler) :: a
      type(c_ptr) :: area
      type(uiAreaDrawParams) :: p
      type(c_ptr) :: path
      type(uiDrawBrush) :: brush
!  fill the area with white
       call d2dSolidBrush(brush, colorWhite, 1.0d0)
       path = uiDrawNewPath(uiDrawFillModeWinding)
       call uiDrawPathAddRectangle(path, 0.0_c_double, 0.0_c_double, p%AreaWidth, p%AreaHeight)
       call uiDrawPathEnd(path)
       call uiDrawFill(p%Context, path, brush)
       call uiDrawFreePath(path)

! // dash
      call drawCSDash(p)

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

!  function handlerKeyEvent(ah, area, e) result(ret)
  function handlerKeyEvent() result(ret)
    use iso_c_binding
    use libuif
!      type(uiAreaHandler) :: ah
!      type(c_ptr) :: area
!      type(uiAreaKeyEvent) :: e
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
  
  call uiOnShouldQuit(c_funloc(shouldQuit), c_null_ptr)

! start procedure

  mainwin = uiNewWindow("libui Draw Dash"//c_null_char, 640, 480, 0)
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
