!
! libui-draw-sample4.f90
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

!// TODO https://msdn.microsoft.com/en-us/library/windows/desktop/dd756676%28v=vs.85%29.aspx

!// TODO? https://msdn.microsoft.com/en-us/library/windows/desktop/dd370971%28v=vs.85%29.aspx

!// TODO are there even examples here? https://msdn.microsoft.com/en-us/library/windows/desktop/dd370966%28v=vs.85%29.aspx

!// from https://msdn.microsoft.com/en-us/library/windows/desktop/dd756687%28v=vs.85%29.aspx
  subroutine drawD2DRotate(p) 
    use iso_c_binding
    use libuif_structs
    use libuif
      type(uiAreaDrawParams) :: p
      type(c_ptr) :: path
      type(uiDrawBrush) :: original
      type(uiDrawBrush) :: fill
      type(uiDrawBrush) :: transform
      type(uiDrawStrokeParams) :: originalsp
      type(uiDrawStrokeParams) :: transformsp
      type(uiDrawMatrix) :: m

        originalsp%Dashes = nilp
        originalsp%NumDashes = 0
        originalsp%DashPhase = 0
        transformsp%Dashes = nilp
        transformsp%NumDashes = 0
        transformsp%DashPhase = 0

        path = uiDrawNewPath(uiDrawFillModeWinding)
        call uiDrawPathAddRectangle(path, 438.0d0, 301.5d0, 498.0d0 - 438.0d0, 361.5d0 - 301.5d0)
        call uiDrawPathEnd(path)

!	// TODO the example doesn't specify what these should be
        call d2dSolidBrush(original, d2dBlack, 1.0d0)
        call d2dSolidBrush(fill, d2dWhite, 0.5d0)
        call d2dSolidBrush(transform, d2dForestGreen, 1.0d0)
!	// TODO this needs to be dashed
         originalsp%Thickness = 1.0
         originalsp%Cap = uiDrawLineCapFlat
         originalsp%Join = uiDrawLineJoinMiter
         originalsp%MiterLimit = uiDrawDefaultMiterLimit
         transformsp%Thickness = 1.0
         transformsp%Cap = uiDrawLineCapFlat
         transformsp%Join = uiDrawLineJoinMiter
         transformsp%MiterLimit = uiDrawDefaultMiterLimit

!	// save for when we do the translated one
         call uiDrawSave(p%Context)

         call uiDrawStroke(p%Context, path, original, originalsp)

         call uiDrawMatrixSetIdentity(m)
         call uiDrawMatrixRotate(m,468.0d0, 331.5d0,45.0d0 * (uiPi / 180.0d0))
         call uiDrawTransform(p%Context, m)

         call uiDrawFill(p%Context, path, fill)
         call uiDrawStroke(p%Context, path, transform, transformsp)

         call uiDrawRestore(p%Context)

!	// translate to test the corner axis
         call uiDrawMatrixSetIdentity(m)
         call uiDrawMatrixTranslate(m, -200.0d0, -200.0d0)
         call uiDrawTransform(p%Context, m)

         call uiDrawStroke(p%Context, path, original, originalsp)

         call uiDrawMatrixSetIdentity(m)
         call uiDrawMatrixRotate(m,438.0d0, 301.5d0,45.0d0 * (uiPi / 180.0d0))
         call uiDrawTransform(p%Context, m)

         call uiDrawFill(p%Context, path, fill)
         call uiDrawStroke(p%Context, path, transform, transformsp)

         call uiDrawFreePath(path)

  end subroutine drawD2DRotate

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

!  drawD2DRotate
      call drawD2DRotate(p)

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
    type(c_ptr) :: mainwin
    type(c_ptr) :: vbox,ebtn
    type(c_ptr) :: drawArea
    type(uiAreaHandler) :: handler


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
    drawArea = uiNewArea(handler)
  call uiBoxAppend(vbox, drawArea, 1)

!  call uiBoxSetPadded(vbox,1)
  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)
  call uiBoxAppend(vbox, ebtn, 0)

  call uiControlShow(mainwin)
  call uiMain()

end program main
