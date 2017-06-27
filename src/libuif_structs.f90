!
! libuif_structs.f90
! Fortran moddule for cross plattform gui library libui
!
! Author Yoji.Hosokawa 2016/4
!
! gfortran -c -O2 -g -Wall libuif_structs.f90
!

module libuif_structs
  use iso_c_binding
  implicit none

! sample of c struct
!  type :: slope_pen_t
!    integer(c_int64_t) :: color
!    real(c_double) :: line_width
!  end type
!
!  type(slope_pen_t) :: pen
!
!  pen%color = 255
!  pen%line_width = 20.0

  type,bind(c) :: uiInitOptions
    integer(c_size_t) :: Size
  end type uiInitOptions

  type,bind(c) :: uiControl
    integer(c_int32_t) :: Signature
    integer(c_int32_t) :: OSSignature
    integer(c_int32_t) :: TypeSignature
    type(c_ptr) :: Destroy
    integer(c_size_t) :: Handle
    type(c_ptr) :: Parent
    type(c_ptr) :: SetParent
    integer(c_int) :: Toplevel
    integer(c_int) :: Visible
    type(c_ptr) :: Show
    type(c_ptr) :: Hide
    integer(c_int) :: Enabled
    type(c_ptr) :: Enable 
    type(c_ptr) :: Disable
  end type uiControl

  type,bind(c) :: uiAreaHandler
    type(c_funptr) :: Draw
    type(c_funptr) :: MouseEvent
    type(c_funptr) :: MouseCrossed
    type(c_funptr) :: DragBroken
    type(c_funptr) :: KeyEvent
  end type uiAreaHandler

  type,bind(c) :: uiAreaDrawParams
    type(c_ptr) :: Context
    real(c_double) :: AreaWidth
    real(c_double) :: AreaHeight
    real(c_double) :: ClipX
    real(c_double) :: ClipY
    real(c_double) :: ClipWidth
    real(c_double) :: ClipHeight
  end type uiAreaDrawParams

  type,bind(c) :: uiDrawMatrix
    real(c_double) :: M11
    real(c_double) :: M12
    real(c_double) :: M21
    real(c_double) :: M22
    real(c_double) :: M31
    real(c_double) :: M32
  end type uiDrawMatrix

  type,bind(c) :: uiDrawBrush
    integer(c_int) :: Type
    real(c_double) :: R
    real(c_double) :: G
    real(c_double) :: B
    real(c_double) :: A
    real(c_double) :: X0
    real(c_double) :: Y0
    real(c_double) :: X1
    real(c_double) :: Y1
    real(c_double) :: OuterRadius
    type(c_ptr) :: Stops
    integer(c_size_t) :: NumStops
  end type uiDrawBrush

  type,bind(c) :: uiDrawBrushGradientStop 
    real(c_double) :: Pos
    real(c_double) :: R
    real(c_double) :: G
    real(c_double) :: B
    real(c_double) :: A
  end type uiDrawBrushGradientStop

  type,bind(c) :: uiDrawStrokeParams
    integer(c_int) :: Cap
    integer(c_int) :: Join
    real(c_double) :: Thickness
    real(c_double) :: MiterLimit
    type(c_ptr) ::  Dashes
    integer(c_size_t) :: NumDashes
    real(c_double) :: DashPhase
  end type uiDrawStrokeParams

  type,bind(c) :: uiDrawTextFontDescriptor 
    type(c_ptr) :: Family
    real(c_double) :: Size
    integer(c_int) :: Weight
    integer(c_int) :: Italic
    integer(c_int) :: Stretch
  end type uiDrawTextFontDescriptor

  type,bind(c) :: uiDrawTextFontMetrics
    real(c_double) :: Ascent
    real(c_double) :: Descent
    real(c_double) :: Leading
    real(c_double) :: UnderlinePos
    real(c_double) :: UnderlineThickness
  end type uiDrawTextFontMetrics

  type,bind(c) :: uiAreaMouseEvent
    real(c_double) :: X
    real(c_double) :: Y
    real(c_double) :: AreaWidth
    real(c_double) :: AreaHeight
    integer(c_int) :: Down
    integer(c_int) :: Up
    integer(c_int) :: Count
    integer(c_int) :: Modifiers
    integer(c_int64_t) :: Held1To64
  end type uiAreaMouseEvent

  type,bind(c) :: uiAreaKeyEvent 
    character(c_char) :: Key
    integer(c_int) :: ExtKey
    integer(c_int) :: Modifier
    integer(c_int) :: Modifiers
    integer(c_int) :: Up
  end type  uiAreaKeyEvent

end module libuif_structs
