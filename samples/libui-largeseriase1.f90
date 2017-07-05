!
! libui-largeserise.f90
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

    real(kind=4) :: xd(1048576), yd(1048576)
    integer,parameter :: NPTS = 1048576

    type(c_ptr) :: mainwin;
    type(c_ptr) :: histogram;
    type(uiAreaHandler) :: handler
    type(uiDrawBrush) :: brush
    type(uiDrawStrokeParams) :: sp
    type(uiDrawMatrix) :: m
    real(c_double) :: graphR, graphG, graphB, graphA
    real(c_double) :: alpha 
!    double precision :: xs(NPTS),ys(NPTS)
    integer :: i
    integer :: currentPoint = 0

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


contains

  ! "close mainwindow" is a this app

  subroutine onClosing()
    use iso_c_binding
    use libuif
      call uiQuit()
  end subroutine onClosing

!  subroutine pointLocations(width, height)
!    use iso_c_binding
!    use libuif
!    implicit none
!      double precision :: width, height
!      double precision :: xincr, yincr, n
!        xincr = width / dble(NPTS - 1)
!        yincr = height / 100.0d0
!        do i = 1, NPTS, 1
!          n = (1.0d0 - dble(yd(i))) * 100.0d0
!          xs(i) = xincr * (i -1)
!          ys(i) = yincr * n
!        end do
!  end subroutine pointLocations

  subroutine setSolidBrush(color, alpha)
    use iso_c_binding
    use libuif
    implicit none
       integer :: color
       double precision :: alpha

         brush%Type = uiDrawBrushTypeSolid
         brush%R = dble(ibits(color,16,8)) / 255.0d0
         brush%G = dble(ibits(color,8,8)) / 255.0d0
         brush%B = dble(ibits(color,0,8)) / 255.0d0
         brush%A = alpha
  end subroutine setSolidBrush

  function constructGraph(width, height, extend) result(path)
    use iso_c_binding
    implicit none
      double precision :: width, height
      integer :: extend, step
      type(c_ptr) :: path

! より高速化＆省メモリ
      double precision :: xincr
!      double precision :: xincr, yincr, n



!        call pointLocations(width, height)

        path = uiDrawNewPath(uiDrawFillModeWinding)

        xincr = width / dble(NPTS - 1)
!        yincr = height / 100.0d0
        call uiDrawPathNewFigure(path, 0.0d0, height * (1.0d0 - dble(yd(1))))

        step = NPTS / int(width)
        if (step < 5) then
          step = 1
        end if 

        do i = 2, NPTS, step
!          n = (1.0d0 - dble(yd(i))) * 100.0d0
!          xs(i) = xincr * (i -1)
!          ys(i) = yincr * n
          call uiDrawPathLineTo(path, xincr * dble(i -1), height * (1.0d0 - dble(yd(i))))
        end do

! draw last point
          call uiDrawPathLineTo(path, xincr * dble(NPTS), height * (1.0d0 - dble(yd(NPTS))))

!        path = uiDrawNewPath(uiDrawFillModeWinding)

!        call uiDrawPathNewFigure(path, xs(1), ys(1))
!! 全ポイントを描画すると処理が遅くなるので、
!! 描画域の幅に比例させる
!        do i = 1, NPTS, NPTS / int(width) + 1
!!        do i = 1, NPTS, 1
!          call uiDrawPathLineTo(path, xs(i), ys(i))
!        end do

        if (extend == 1) then
          call uiDrawPathLineTo(path, dble(width), dble(height / 2.0d0))
          call uiDrawPathLineTo(path, 0.0d0, dble(height / 2.0d0))
!          call uiDrawPathLineTo(path, dble(width), dble(height))
!          call uiDrawPathLineTo(path, 0.0d0, dble(height))
          call uiDrawPathCloseFigure(path)
        end if

        call uiDrawPathEnd(path)
  end function constructGraph


  subroutine graphSize(clientWidth, clientHeight,graphWidth, graphHeight)
    use iso_c_binding
    implicit none
      double precision :: clientWidth, clientHeight
      double precision :: graphWidth, graphHeight
        graphWidth = clientWidth - dble(xoffLeft) - dble(xoffRight)
        graphHeight = clientHeight - dble(yoffTop) - dble(yoffBottom)
  end subroutine graphSize

  subroutine handlerDraw(a, area, p)
    use iso_c_binding
    use libuif_structs
    use libuif
    implicit none
      type(uiAreaHandler) :: a
      type(c_ptr) :: area
      type(uiAreaDrawParams) :: p
      type(c_ptr) :: path
      double precision :: graphWidth, graphHeight

!  fill the area with white
       call setSolidBrush(colorWhite, 1.0d0)
       path = uiDrawNewPath(uiDrawFillModeWinding)
       call uiDrawPathAddRectangle(path, 0.0d0, 0.0d0, p%AreaWidth, p%AreaHeight)
       call uiDrawPathEnd(path)
       call uiDrawFill(p%Context, path, brush)
       call uiDrawFreePath(path)

!  figure out dimensions
       call graphSize(p%AreaWidth, p%AreaHeight, graphWidth, graphHeight)

!  clear sp to avoid passing garbage to uiDrawStroke()
!  for example, we don't use dashing
! memset(&sp, 0, sizeof (uiDrawStrokeParams));

!   make a stroke for both the axes and the histogram line
       sp%Cap = uiDrawLineCapFlat
       sp%Join = uiDrawLineJoinMiter
       sp%Thickness = 2.0d0
       sp%MiterLimit = uiDrawDefaultMiterLimit

!   draw the axes
       call setSolidBrush(colorBlack, 1.0d0)
       path = uiDrawNewPath(uiDrawFillModeWinding)
       call uiDrawPathNewFigure(path,dble(xoffLeft), dble(yoffTop))
       call uiDrawPathLineTo(path, dble(xoffLeft), dble(yoffTop + graphHeight))
       call uiDrawPathNewFigure(path,dble(xoffLeft), dble(yoffTop + graphHeight / 2.0d0))
       call uiDrawPathLineTo(path, dble(xoffLeft) + graphWidth, dble(yoffTop) + graphHeight / 2.0d0)
!       call uiDrawPathLineTo(path, dble(xoffLeft + graphWidth), dble(yoffTop + graphHeight))
       call uiDrawPathEnd(path)
       call uiDrawStroke(p%Context, path, brush, sp)
       call uiDrawFreePath(path)

!   now transform the coordinate space so (0, 0) is the top-left corner of the graph
       call uiDrawMatrixSetIdentity(m)
       call uiDrawMatrixTranslate(m, dble(xoffLeft), dble(yoffTop))
       call uiDrawTransform(p%Context, m)

!   we set brush->A below to different values for the fill and stroke
       call setSolidBrush(colorDodgerBlue, 1.0d0)

!!   now create the fill for the graph below the graph line
!       path = constructGraph(graphWidth, graphHeight, 1)

!       brush%A = 1.0d0 / 2.0d0
!       call uiDrawFill(p%Context, path, brush)
!       call uiDrawFreePath(path)

!   now draw the histogram line
       path = constructGraph(graphWidth, graphHeight, 0)
       brush%A = 1.0d0
       call uiDrawStroke(p%Context, path, brush, sp)
       call uiDrawFreePath(path)

!!   now draw the point being hovered over
!       if (currentPoint /= 0) then 
!          path = uiDrawNewPath(uiDrawFillModeWinding)
!          call  uiDrawPathNewFigureWithArc(path,xs(currentPoint),ys(currentPoint),dble(pointRadius),0.0d0,6.23d0,0)
!          call  uiDrawPathEnd(path)
!   use the same brush as for the histogram lines
!          call  uiDrawFill(p%Context, path, brush)
!          call  uiDrawFreePath(path)
!        end if
  end subroutine handlerDraw

!  function inPoint(x0, y0, xtest, ytest) result(ret)
!    use iso_c_binding
!    use libuif
!      double precision :: X0, Y0, x, y, xtest, ytest
!      logical :: ret
!         x = x0 - dble(xoffLeft)
!         y = y0 - dble(yoffTop)
!         ret = (x >= (xtest - dble(pointRadius))) .and. (x <= (xtest + dble(pointRadius))) .and. &
!&           (y >= (ytest - dble(pointRadius))) .and. (y <= (ytest + dble(pointRadius)))
!  end function inPoint

  subroutine handlerMouseEvent(a, area, e)
! データ数が多すぎて処理が遅くなるので、
! 何もしないとする
    use iso_c_binding
    use libuif_structs
    use libuif
    implicit none
      type(uiAreaHandler) :: a
      type(c_ptr) :: area
      type(uiAreaMouseEvent) :: e

!        do i = 1, NPTS, 1
!          if (inPoint(e%X, e%Y, xs(i), ys(i))) then
!            exit
!          end if
!        end do
!        if (i == (NPTS + 1)) then   ! not in a point
!           i = 0
!        end if

!        currentPoint = i
!   TODO only redraw the relevant area
!        call uiAreaQueueRedrawAll(histogram)

!   do nothing

  end subroutine handlerMouseEvent

  subroutine handlerMouseCrossed() 
    use iso_c_binding
    use libuif
!   do nothing
  end subroutine handlerMouseCrossed 

  subroutine handlerDragBroken()
    use iso_c_binding
    use libuif
!   do nothing
  end subroutine handlerDragBroken

  function handlerKeyEvent() result(ret)
    use iso_c_binding
    use libuif
      integer :: ret
!   do nothing
        ret = 0
  end function handlerKeyEvent

end module handlers

program main
    use iso_c_binding
    use libuif_structs
    use libuif
    use handlers

    implicit none
    type(c_ptr) :: vbox,ebtn
!    real(kind=4) :: step
    real(kind=4) :: NPTSR


! initialize
  call uiInit(0)


!  step = 2.0d0 * uiPi / dble( NPTS - 1 )
!  do i=1, NPTS, 1
!    xd(i) = real((i - 1) * step)
!    yd(i) = (sin(xd(i)) + 1.0) / 2.0  
!  end do

  NPTSR = NPTS
  do i = 1, NPTS, 1
    xd(i) = real(i - 1)
    yd(i) = (sin( real(i) * 33.0 * real(uiPi) / NPTSR) + sin(real(i) * 15.0 * real(uiPi) / NPTSR) + 2.0) / 4.0
  end do

! start procedure

  mainwin = uiNewWindow("libui Large Serieas"//c_null_char, 700, 600, 0)
  call uiWindowSetMargined(mainwin, 1)
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

  call uiBoxSetPadded(vbox,1)
  ebtn = uiNewButton("終了"//c_null_char)
  call uiButtonOnClicked(ebtn, c_funloc(onClosing), c_null_ptr)
  call uiBoxAppend(vbox, ebtn, 0)

  call uiControlShow(mainwin)
  call uiMain()

! nomary do not need if call when close window receive sig fault by button clicked
!  call uiUninit()

end program main
