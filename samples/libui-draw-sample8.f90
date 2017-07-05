!
! libui-draw-sample8.f90
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

!// from https://msdn.microsoft.com/en-us/library/windows/desktop/dd756672%28v=vs.85%29.aspx
!// TODO the points seem off
  subroutine drawD2DMultiTransforms(p)
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
      type(uiDrawMatrix) :: mtranslate
      type(uiDrawMatrix) :: mrotate

        originalsp%Dashes = nilp
        originalsp%NumDashes = 0
        originalsp%DashPhase = 0
        transformsp%Dashes = nilp
        transformsp%NumDashes = 0
        transformsp%DashPhase = 0

          path = uiDrawNewPath(uiDrawFillModeWinding)
          call uiDrawPathAddRectangle(path, 300.0d0, 40.0d0, 360.0d0 - 300.0d0, 100.0d0 - 40.0d0)
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

          call uiDrawMatrixSetIdentity(mtranslate)
          call uiDrawMatrixTranslate(mtranslate, 20.0d0, 10.0d0)
          call uiDrawMatrixSetIdentity(mrotate)
          call uiDrawMatrixRotate(mrotate,330.0d0, 70.0d0,45.0d0 * (uiPi / 180.0d0))

!	// save for when we do the opposite one
          call uiDrawSave(p%Context)

          call uiDrawStroke(p%Context, path, original, originalsp)

          call uiDrawTransform(p%Context, mrotate)
          call uiDrawTransform(p%Context, mtranslate)

          call uiDrawFill(p%Context, path, fill)
          call uiDrawStroke(p%Context, path, transform, transformsp)

          call uiDrawRestore(p%Context)
          call uiDrawFreePath(path)

          path = uiDrawNewPath(uiDrawFillModeWinding)
          call uiDrawPathAddRectangle(path, 40.0d0, 40.0d0, 100.0d0 - 40.0d0, 100.0d0 - 40.0d0)
          call uiDrawPathEnd(path)

          call uiDrawMatrixSetIdentity(mtranslate)
          call uiDrawMatrixTranslate(mtranslate, 20.0d0, 10.0d0)
          call uiDrawMatrixSetIdentity(mrotate)
          call uiDrawMatrixRotate(mrotate,70.0d0, 70.0d0,45.0d0 * (uiPi / 180.0d0))

          call uiDrawStroke(p%Context, path, original, originalsp)

          call uiDrawTransform(p%Context, mtranslate)
          call uiDrawTransform(p%Context, mrotate)

          call uiDrawFill(p%Context, path, fill)
          call uiDrawStroke(p%Context, path, transform, transformsp)

          call uiDrawFreePath(path)

  end subroutine drawD2DMultiTransforms

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

!  drawD2DMultiTransforms
      call drawD2DMultiTransforms(p)

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
