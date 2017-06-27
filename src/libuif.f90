!
! libuif.f90
! Fortran Module for cross plattform gui library libui
!
! Author Yoji.Hosokawa 2016/4
!
! gfortran -c -O2 -g -Wall libuif.f90
!

module libuif
  use iso_c_binding
!  use libuif_structs
  implicit none

    character(c_char),parameter :: nilc = c_null_char
    type(c_ptr),parameter :: nilp = c_null_ptr
     
    real(c_double),parameter :: uiPi = 3.14159265358979323846264338327950288419716939937510582097494459

    !_UI_ENUM(uiWindowResizeEdge) 
    integer(c_int), parameter :: uiWindowResizeEdgeLeft = 0
    integer(c_int), parameter :: uiWindowResizeEdgeTop = 1
    integer(c_int), parameter :: uiWindowResizeEdgeRight = 2
    integer(c_int), parameter :: uiWindowResizeEdgeBottom = 3
    integer(c_int), parameter :: uiWindowResizeEdgeTopLeft = 4
    integer(c_int), parameter :: uiWindowResizeEdgeTopRight = 5
    integer(c_int), parameter :: uiWindowResizeEdgeBottomLeft = 6
    integer(c_int), parameter :: uiWindowResizeEdgeBottomRight = 7

    !_UI_ENUM(uiDrawBrushType) 
    integer(c_int), parameter :: uiDrawBrushTypeSolid = 0
    integer(c_int), parameter :: uiDrawBrushTypeLinearGradient = 1
    integer(c_int), parameter :: uiDrawBrushTypeRadialGradient = 2
    integer(c_int), parameter :: uiDrawBrushTypeImage = 3

    !_UI_ENUM(uiDrawLineCap)
    integer(c_int), parameter :: uiDrawLineCapFlat = 0
    integer(c_int), parameter :: uiDrawLineCapRound = 1
    integer(c_int), parameter :: uiDrawLineCapSquare = 2

    !_UI_ENUM(uiDrawLineJoin) 
    integer(c_int), parameter :: uiDrawLineJoinMiter = 0
    integer(c_int), parameter :: uiDrawLineJoinRound = 1
    integer(c_int), parameter :: uiDrawLineJoinBevel = 2

    !#define uiDrawDefaultMiterLimit 10.0
    double precision, parameter :: uiDrawDefaultMiterLimit = 10.0d0

    !_UI_ENUM(uiDrawFillMode) 
    integer(c_int), parameter :: uiDrawFillModeWinding = 0
    integer(c_int), parameter :: uiDrawFillModeAlternate = 1

    !_UI_ENUM(uiDrawTextWeight)
    integer(c_int), parameter :: uiDrawTextWeightThin = 0
    integer(c_int), parameter :: uiDrawTextWeightUltraLight = 1
    integer(c_int), parameter :: uiDrawTextWeightLight = 2
    integer(c_int), parameter :: uiDrawTextWeightBook = 3
    integer(c_int), parameter :: uiDrawTextWeightNormal = 4
    integer(c_int), parameter :: uiDrawTextWeightMedium = 5
    integer(c_int), parameter :: uiDrawTextWeightSemiBold = 6
    integer(c_int), parameter :: uiDrawTextWeightBold = 7
    integer(c_int), parameter :: uiDrawTextWeightUltraBold = 8
    integer(c_int), parameter :: uiDrawTextWeightHeavy = 9
    integer(c_int), parameter :: uiDrawTextWeightUltraHeavy = 10

    !_UI_ENUM(uiDrawTextItalic) 
    integer(c_int), parameter :: uiDrawTextItalicNormal = 0
    integer(c_int), parameter :: uiDrawTextItalicOblique = 1
    integer(c_int), parameter :: uiDrawTextItalicItalic =2

    !_UI_ENUM(uiDrawTextStretch)
    integer(c_int), parameter :: uiDrawTextStretchUltraCondensed = 0
    integer(c_int), parameter :: uiDrawTextStretchExtraCondensed = 1
    integer(c_int), parameter :: uiDrawTextStretchCondensed =2
    integer(c_int), parameter :: uiDrawTextStretchSemiCondensed = 3
    integer(c_int), parameter :: uiDrawTextStretchNormal = 4
    integer(c_int), parameter :: uiDrawTextStretchSemiExpanded = 5
    integer(c_int), parameter :: uiDrawTextStretchExpanded = 6
    integer(c_int), parameter :: uiDrawTextStretchExtraExpanded = 7
    integer(c_int), parameter :: uiDrawTextStretchUltraExpanded = 8

    !_UI_ENUM(uiModifiers)
    integer(c_int), parameter :: uiModifierCtrl = 1 
    integer(c_int), parameter :: uiModifierAlt = 2
    integer(c_int), parameter :: uiModifierShift = 4
    integer(c_int), parameter :: uiModifierSuper = 8

    !_UI_ENUM(uiExtKey) 
    integer(c_int), parameter :: uiExtKeyEscape = 1
    integer(c_int), parameter :: uiExtKeyInsert = 2
    integer(c_int), parameter :: uiExtKeyDelete = 3
    integer(c_int), parameter :: uiExtKeyHome = 4
    integer(c_int), parameter :: uiExtKeyEnd = 5
    integer(c_int), parameter :: uiExtKeyPageUp = 6
    integer(c_int), parameter :: uiExtKeyPageDown = 7
    integer(c_int), parameter :: uiExtKeyUp = 8
    integer(c_int), parameter :: uiExtKeyDown = 9
    integer(c_int), parameter :: uiExtKeyLeft = 10
    integer(c_int), parameter :: uiExtKeyRight = 11
    integer(c_int), parameter :: uiExtKeyF1 = 12
    integer(c_int), parameter :: uiExtKeyF2 = 13
    integer(c_int), parameter :: uiExtKeyF3 = 14
    integer(c_int), parameter :: uiExtKeyF4 = 15
    integer(c_int), parameter :: uiExtKeyF5 = 16
    integer(c_int), parameter :: uiExtKeyF6 = 17
    integer(c_int), parameter :: uiExtKeyF7 = 18
    integer(c_int), parameter :: uiExtKeyF8 = 19
    integer(c_int), parameter :: uiExtKeyF9 = 20
    integer(c_int), parameter :: uiExtKeyF10 = 21
    integer(c_int), parameter :: uiExtKeyF11 = 22
    integer(c_int), parameter :: uiExtKeyF12 = 23
    integer(c_int), parameter :: uiExtKeyN0 = 24
    integer(c_int), parameter :: uiExtKeyN1 = 25
    integer(c_int), parameter :: uiExtKeyN2 = 26
    integer(c_int), parameter :: uiExtKeyN3 = 27
    integer(c_int), parameter :: uiExtKeyN4 = 28
    integer(c_int), parameter :: uiExtKeyN5 = 29
    integer(c_int), parameter :: uiExtKeyN6 = 30
    integer(c_int), parameter :: uiExtKeyN7 = 31
    integer(c_int), parameter :: uiExtKeyN8 = 32
    integer(c_int), parameter :: uiExtKeyN9 = 33
    integer(c_int), parameter :: uiExtKeyNDot = 34
    integer(c_int), parameter :: uiExtKeyNEnter = 35
    integer(c_int), parameter :: uiExtKeyNAdd = 36
    integer(c_int), parameter :: uiExtKeyNSubtract = 37
    integer(c_int), parameter :: uiExtKeyNMultiply = 38
    integer(c_int), parameter :: uiExtKeyNDivide = 39

    !_UI_ENUM(uiAlign)
    integer(c_int), parameter :: uiAlignFill = 0
    integer(c_int), parameter :: uiAlignStart = 1
    integer(c_int), parameter :: uiAlignCenter = 2
    integer(c_int), parameter :: uiAlignEnd = 3

    !_UI_ENUM(uiAt)
    integer(c_int), parameter :: uiAtLeading = 0
    integer(c_int), parameter :: uiAtTop = 1
    integer(c_int), parameter :: uiAtTrailing = 2
    integer(c_int), parameter :: uiAtBottom = 3

interface

!
! from ui.h
!
!_UI_EXTERN const char *uiInit(uiInitOptions *options);
  subroutine uiInit(options) BIND(C, name='uiInit')
    use iso_c_binding
    integer(c_int) :: options
  end subroutine uiInit

!_UI_EXTERN void uiUninit(void);
  subroutine uiUninit() BIND(C, name='uiUninit')
    use iso_c_binding
  end subroutine uiUninit
  
!_UI_EXTERN void uiFreeInitError(const char *err);
  subroutine uiFreeInitError(uierr) BIND(C, name='uiFreeInitError')
    use iso_c_binding
    character(c_char), dimension(*), intent(in) :: uierr 
  end subroutine uiFreeInitError

!_UI_EXTERN void uiMain(void);
  subroutine uiMain() BIND(C, name='uiMain')
    use iso_c_binding
  end subroutine uiMain
  
!_UI_EXTERN void uiMainSteps(void);
  subroutine uiMainSteps() BIND(C, name='uiMainSteps')
    use iso_c_binding
  end subroutine uiMainSteps

!_UI_EXTERN int uiMainStep(int wait);
  function uiMainStep(uiwait) result(ret) BIND(C, name='uiMainStep')
    use iso_c_binding
    integer(c_int), value :: uiwait
    integer(c_int) :: ret
  end function uiMainStep

!_UI_EXTERN void uiQuit(void);
  subroutine uiQuit() BIND(C, name='uiQuit')
    use iso_c_binding
  end subroutine uiQuit

!_UI_EXTERN void uiQueueMain(void (*f)(void *data), void *data);
  subroutine uiQueueMain(fdata, uidata) BIND(C, name='uiQueueMain')
    use iso_c_binding
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiQueueMain

!_UI_EXTERN void uiOnShouldQuit(int (*f)(void *data), void *data);
  subroutine uiOnShouldQuit(fdata, uidata) BIND(C, name='uiOnShouldQuit')
    use iso_c_binding
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiOnShouldQuit

!_UI_EXTERN void uiFreeText(char *text);
  subroutine uiFreeText(text) BIND(C, name='uiFreeText')
    use iso_c_binding
    character(c_char), dimension(*), intent(in) :: text 
  end subroutine uiFreeText

!_UI_EXTERN void uiControlDestroy(uiControl *);
  subroutine uiControlDestroy(obj) BIND(C, name='uiControlDestroy')
    use iso_c_binding
    type(c_ptr), value :: obj
  end subroutine uiControlDestroy

!_UI_EXTERN uintptr_t uiControlHandle(uiControl *);
  function uiControlHandle(obj) result(ret) BIND(C, name='uiControlHandle')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), pointer :: ret
  end function uiControlHandle

!_UI_EXTERN uiControl *uiControlParent(uiControl *);
  function uiControlParent(obj) result(ret) BIND(C, name='uiControlParent')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiControlParent

!_UI_EXTERN void uiControlSetParent(uiControl *, uiControl *);
  subroutine uiControlSetParent(obj1, obj2) BIND(C, name='uiControlSetParent')
    use iso_c_binding
    type(c_ptr), value :: obj1
    type(c_ptr), value :: obj2
  end subroutine uiControlSetParent

!_UI_EXTERN int uiControlToplevel(uiControl *);
  function uiControlToplevel(obj) result(ret) BIND(C, name='uiControlToplevel')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiControlToplevel

!_UI_EXTERN int uiControlVisible(uiControl *);
  function uiControlVisible(obj) result(ret) BIND(C, name='uiControlVisible')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiControlVisible

!_UI_EXTERN void uiControlShow(uiControl *);
  subroutine uiControlShow(obj) BIND(C, name='uiControlShow')
    use iso_c_binding
    type(c_ptr), value :: obj
  end subroutine uiControlShow

!_UI_EXTERN void uiControlHide(uiControl *);
  subroutine uiControlHide(obj) BIND(C, name='uiControlHide')
    use iso_c_binding
    type(c_ptr), value :: obj
  end subroutine uiControlHide

!_UI_EXTERN int uiControlEnabled(uiControl *);
  function uiControlEnabled(obj) result(ret) BIND(C, name='uiControlEnabled')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiControlEnabled

!_UI_EXTERN void uiControlEnable(uiControl *);
  subroutine uiControlEnable(obj) BIND(C, name='uiControlEnable')
    use iso_c_binding
    type(c_ptr), value :: obj
  end subroutine uiControlEnable

!_UI_EXTERN void uiControlDisable(uiControl *);
  subroutine uiControlDisable(obj) BIND(C, name='uiControlDisable')
    use iso_c_binding
    type(c_ptr), value :: obj
  end subroutine uiControlDisable

!_UI_EXTERN uiControl *uiAllocControl(size_t n, uint32_t OSsig, uint32_t typesig, const char *typenamestr);
  function uiAllocControl(n, OSsig, typesig, typenamestr) result(ret) BIND(C, name='uiAllocControl')
    use iso_c_binding
    integer(c_size_t), value :: n
    integer(c_int32_t), value :: OSsig
    integer(c_int32_t), value :: typesig
    character(c_char), dimension(*), intent(in) :: typenamestr 
    type(c_ptr) :: ret
  end function uiAllocControl

!_UI_EXTERN void uiFreeControl(uiControl *);
  subroutine uiFreeControl(obj) BIND(C, name='uiFreeControl')
    use iso_c_binding
    type(c_ptr), value :: obj
  end subroutine uiFreeControl

!_UI_EXTERN char *uiWindowTitle(uiWindow *w);
  function uiWindowTitle(obj) result(ret) BIND(C, name='uiWindowTitle')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiWindowTitle

!_UI_EXTERN void uiWindowSetTitle(uiWindow *w, const char *title);
  subroutine uiWindowSetTitle(obj, title) BIND(C, name='uiWindowSetTitle')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: title
  end subroutine uiWindowSetTitle

!_UI_EXTERN void uiWindowContentSize(uiWindow *w, int *width, int *height);
  subroutine uiWindowContentSize(obj, width, height) BIND(C, name='uiWindowContentSize')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: width, height
  end subroutine uiWindowContentSize

!_UI_EXTERN void uiWindowSetContentSize(uiWindow *w, int width, int height);
  subroutine uiWindowSetContentSize(obj, width, height) BIND(C, name='uiWindowSetContentSize')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: width, height
  end subroutine uiWindowSetContentSize

!_UI_EXTERN int uiWindowFullscreen(uiWindow *w);
  function uiWindowFullscreen(obj) result(ret) BIND(C, name='uiWindowFullscreen')
    use iso_c_binding
    type(c_ptr) :: obj
    integer(c_int) :: ret
  end function uiWindowFullscreen

!_UI_EXTERN void uiWindowSetFullscreen(uiWindow *w, int fullscreen);
  subroutine uiWindowSetFullscreen(obj, fullscreen) BIND(C, name='uiWindowSetFullscreen')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: fullscreen
  end subroutine uiWindowSetFullscreen

!_UI_EXTERN void uiWindowOnContentSizeChanged(uiWindow *w, void (*f)(uiWindow *, void *), void *data);
  subroutine uiWindowOnContentSizeChanged(obj, obj1, uidata) BIND(C, name='uiWindowOnContentSizeChanged')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_funptr), value :: obj1
    type(c_ptr), value :: uidata
  end subroutine uiWindowOnContentSizeChanged

!_UI_EXTERN void uiWindowOnClosing(uiWindow *w, int (*f)(uiWindow *w, void *data), void *data);
  subroutine uiWindowOnClosing(obj, fdata, uidata) BIND(C, name='uiWindowOnClosing')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiWindowOnClosing

!_UI_EXTERN int uiWindowBorderless(uiWindow *w);
  function uiWindowBorderless(obj) result(ret) BIND(C, name='uiWindowBorderless')
    use iso_c_binding
    type(c_ptr) :: obj
    integer(c_int) :: ret
  end function uiWindowBorderless

!_UI_EXTERN void uiWindowSetBorderless(uiWindow *w, int borderless);
  subroutine uiWindowSetBorderless(obj, borderless) BIND(C, name='uiWindowSetBorderless')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: borderless
  end subroutine uiWindowSetBorderless

!_UI_EXTERN void uiWindowSetChild(uiWindow *w, uiControl *child);
  subroutine uiWindowSetChild(obj, obj1) BIND(C, name='uiWindowSetChild')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr), value :: obj1
  end subroutine uiWindowSetChild

!_UI_EXTERN int uiWindowMargined(uiWindow *w);
  function uiWindowMargined(obj) result(ret) BIND(C, name='uiWindowMargined')
    use iso_c_binding
    type(c_ptr) :: obj
    integer(c_int) :: ret
  end function uiWindowMargined

!_UI_EXTERN void uiWindowSetMargined(uiWindow *w, int margined);
  subroutine uiWindowSetMargined(obj, margined) BIND(C, name='uiWindowSetMargined')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: margined
  end subroutine uiWindowSetMargined

!_UI_EXTERN uiWindow *uiNewWindow(const char *title, int width, int height, int hasMenubar);
  function uiNewWindow(title, width, height, hasMenubar) result(ret) BIND(C, name='uiNewWindow')
    use iso_c_binding
    character(c_char), dimension(*), intent(in) :: title
    integer(c_int), value :: width, height, hasMenubar
    type(c_ptr) :: ret
  end function uiNewWindow

!_UI_EXTERN char *uiButtonText(uiButton *b);
  function uiButtonText(obj) result(ret) BIND(C, name='uiButtonText')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiButtonText

!_UI_EXTERN void uiButtonSetText(uiButton *b, const char *text);
  subroutine uiButtonSetText(obj, text) BIND(C, name='uiButtonSetText')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: text
  end subroutine uiButtonSetText

!_UI_EXTERN void uiButtonOnClicked(uiButton *b, void (*f)(uiButton *b, void *data), void *data);
  subroutine uiButtonOnClicked(obj, fdata, uidata) BIND(C, name='uiButtonOnClicked')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiButtonOnClicked

!_UI_EXTERN uiButton *uiNewButton(const char *text);
  function uiNewButton(text) result(ret) BIND(C, name='uiNewButton')
    use iso_c_binding
    character(c_char), dimension(*), intent(in) :: text
    type(c_ptr) :: ret
  end function uiNewButton

!_UI_EXTERN void uiBoxAppend(uiBox *b, uiControl *child, int stretchy);
  subroutine uiBoxAppend(obj, obj1, stretchy) BIND(C, name='uiBoxAppend')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr), value :: obj1
    integer(c_int), value :: stretchy
  end subroutine uiBoxAppend

!_UI_EXTERN void uiBoxDelete(uiBox *b, int index);
  subroutine uiBoxDelete(obj, uiindex) BIND(C, name='uiBoxDelete')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: uiindex
  end subroutine uiBoxDelete

!_UI_EXTERN int uiBoxPadded(uiBox *b);
  function uiBoxPadded(obj) result(ret) BIND(C, name='uiBoxPadded')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiBoxPadded

!_UI_EXTERN void uiBoxSetPadded(uiBox *b, int padded);
  subroutine uiBoxSetPadded(obj, padded) BIND(C, name='uiBoxSetPadded')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: padded
  end subroutine uiBoxSetPadded

!_UI_EXTERN uiBox *uiNewHorizontalBox(void);
  function uiNewHorizontalBox() result(ret) BIND(C, name='uiNewHorizontalBox')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewHorizontalBox

!_UI_EXTERN uiBox *uiNewVerticalBox(void);
  function uiNewVerticalBox() result(ret) BIND(C, name='uiNewVerticalBox')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewVerticalBox

!_UI_EXTERN char *uiCheckboxText(uiCheckbox *c);
  function uiCheckboxText(obj) result(ret) BIND(C, name='uiCheckboxText')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiCheckboxText

!_UI_EXTERN void uiCheckboxSetText(uiCheckbox *c, const char *text);
  subroutine uiCheckboxSetText(obj, text) BIND(C, name='uiCheckboxSetText')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: text
  end subroutine uiCheckboxSetText

!_UI_EXTERN void uiCheckboxOnToggled(uiCheckbox *c, void (*f)(uiCheckbox *c, void *data), void *data);
  subroutine uiCheckboxOnToggled(obj, fdata, uidata) BIND(C, name='uiCheckboxOnToggled')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiCheckboxOnToggled

!_UI_EXTERN int uiCheckboxChecked(uiCheckbox *c);
  function uiCheckboxChecked(obj) result(ret) BIND(C, name='uiCheckboxChecked')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiCheckboxChecked

!_UI_EXTERN void uiCheckboxSetChecked(uiCheckbox *c, int checked);
  subroutine uiCheckboxSetChecked(obj, checked) BIND(C, name='uiCheckboxSetChecked')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: checked
  end subroutine uiCheckboxSetChecked

!_UI_EXTERN uiCheckbox *uiNewCheckbox(const char *text);
  function uiNewCheckbox(text) result(ret)  BIND(C, name='uiNewCheckbox')
    use iso_c_binding
    character(c_char), dimension(*), intent(in) :: text
    type(c_ptr) :: ret
  end function uiNewCheckbox

!_UI_EXTERN char *uiEntryText(uiEntry *e);
  function uiEntryText(obj) result(ret) BIND(C, name='uiEntryText')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiEntryText

!_UI_EXTERN void uiEntrySetText(uiEntry *e, const char *text);
  subroutine uiEntrySetText(obj, text) BIND(C, name='uiEntrySetText')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: text
  end subroutine uiEntrySetText

!_UI_EXTERN void uiEntryOnChanged(uiEntry *e, void (*f)(uiEntry *e, void *data), void *data);
  subroutine uiEntryOnChanged(obj, fdata, uidata) BIND(C, name='uiEntryOnChanged')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiEntryOnChanged

!_UI_EXTERN int uiEntryReadOnly(uiEntry *e);
  function uiEntryReadOnly(obj) result(ret) BIND(C, name='uiEntryReadOnly')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiEntryReadOnly

!_UI_EXTERN void uiEntrySetReadOnly(uiEntry *e, int readonly);
  subroutine uiEntrySetReadOnly(obj, readonly) BIND(C, name='uiEntrySetReadOnly')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: readonly
  end subroutine uiEntrySetReadOnly

!_UI_EXTERN uiEntry *uiNewEntry(void);
  function uiNewEntry() result(ret)  BIND(C, name='uiNewEntry')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewEntry

!_UI_EXTERN uiEntry *uiNewPasswordEntry(void);
  function uiNewPasswordEntry() result(ret)  BIND(C, name='uiNewPasswordEntry')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewPasswordEntry

!_UI_EXTERN uiEntry *uiNewSearchEntry(void);
  function uiNewSearchEntry() result(ret)  BIND(C, name='uiNewSearchEntry')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewSearchEntry

!_UI_EXTERN char *uiLabelText(uiLabel *l);
  function uiLabelText(obj) result(ret) BIND(C, name='uiLabelText')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiLabelText

!_UI_EXTERN void uiLabelSetText(uiLabel *l, const char *text);
  subroutine uiLabelSetText(obj, text) BIND(C, name='uiLabelSetText')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: text
  end subroutine uiLabelSetText

!_UI_EXTERN uiLabel *uiNewLabel(const char *text);
  function uiNewLabel(text) result(ret)  BIND(C, name='uiNewLabel')
    use iso_c_binding
    character(c_char), dimension(*), intent(in) :: text
    type(c_ptr) :: ret
  end function uiNewLabel

!_UI_EXTERN void uiTabAppend(uiTab *t, const char *name, uiControl *c);
  subroutine uiTabAppend(obj, uiname, obj1) BIND(C, name='uiTabAppend')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: uiname
    type(c_ptr), value :: obj1
  end subroutine uiTabAppend

!_UI_EXTERN void uiTabInsertAt(uiTab *t, const char *name, int before, uiControl *c);
  subroutine uiTabInsertAt(obj, uiname, before, obj1) BIND(C, name='uiTabInsertAt')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: uiname
    integer(c_int), value :: before
    type(c_ptr), value :: obj1
  end subroutine uiTabInsertAt

!_UI_EXTERN void uiTabDelete(uiTab *t, int index);
  subroutine uiTabDelete(obj, uiindex) BIND(C, name='uiTabDelete')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: uiindex
  end subroutine uiTabDelete

!_UI_EXTERN int uiTabNumPages(uiTab *t);
  function uiTabNumPages(obj) result(ret) BIND(C, name='uiTabNumPages')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiTabNumPages

!_UI_EXTERN int uiTabMargined(uiTab *t, int page);
  function uiTabMargined(obj, page) result(ret) BIND(C, name='uiTabMargined')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: page
    integer(c_int) :: ret
  end function uiTabMargined

!_UI_EXTERN void uiTabSetMargined(uiTab *t, int page, int margined);
  subroutine uiTabSetMargined(obj, page, margined) BIND(C, name='uiTabSetMargined')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: page
    integer(c_int), value :: margined
  end subroutine uiTabSetMargined

!_UI_EXTERN uiTab *uiNewTab(void);
  function uiNewTab() result(ret)  BIND(C, name='uiNewTab')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewTab

!_UI_EXTERN char *uiGroupTitle(uiGroup *g);
  function uiGroupTitle(obj) result(ret) BIND(C, name='uiGroupTitle')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiGroupTitle

!_UI_EXTERN void uiGroupSetTitle(uiGroup *g, const char *title);
  subroutine uiGroupSetTitle(obj, title) BIND(C, name='uiGroupSetTitle')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: title
  end subroutine uiGroupSetTitle

!_UI_EXTERN void uiGroupSetChild(uiGroup *g, uiControl *c);
  subroutine uiGroupSetChild(obj, obj1) BIND(C, name='uiGroupSetChild')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr), value :: obj1
  end subroutine uiGroupSetChild

!_UI_EXTERN int uiGroupMargined(uiGroup *g);
  function uiGroupMargined(obj) result(ret) BIND(C, name='uiGroupMargined')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiGroupMargined

!_UI_EXTERN void uiGroupSetMargined(uiGroup *g, int margined);
  subroutine uiGroupSetMargined(obj, margined) BIND(C, name='uiGroupSetMargined')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: margined
  end subroutine uiGroupSetMargined

!_UI_EXTERN uiGroup *uiNewGroup(const char *title);
  function uiNewGroup(title) result(ret)  BIND(C, name='uiNewGroup')
    use iso_c_binding
    character(c_char), dimension(*), intent(in) :: title
    type(c_ptr) :: ret
  end function uiNewGroup

!_UI_EXTERN int uiSpinboxValue(uiSpinbox *s);
  function uiSpinboxValue(obj) result(ret) BIND(C, name='uiSpinboxValue')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiSpinboxValue

!_UI_EXTERN void uiSpinboxSetValue(uiSpinbox *s, int value);
  subroutine uiSpinboxSetValue(obj, uivalue) BIND(C, name='uiSpinboxSetValue')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: uivalue
  end subroutine uiSpinboxSetValue

!_UI_EXTERN void uiSpinboxOnChanged(uiSpinbox *s, void (*f)(uiSpinbox *s, void *data), void *data);
  subroutine uiSpinboxOnChanged(obj, fdata, uidata) BIND(C, name='uiSpinboxOnChanged')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiSpinboxOnChanged

!_UI_EXTERN uiSpinbox *uiNewSpinbox(int min, int max);
  function uiNewSpinbox(uimin, uimax) result(ret)  BIND(C, name='uiNewSpinbox')
    use iso_c_binding
    integer(c_int), value :: uimin, uimax
    type(c_ptr) :: ret
  end function uiNewSpinbox

!_UI_EXTERN int uiSliderValue(uiSlider *s);
  function uiSliderValue(obj) result(ret) BIND(C, name='uiSliderValue')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiSliderValue

!_UI_EXTERN void uiSliderSetValue(uiSlider *s, int value);
  subroutine uiSliderSetValue(obj, uivalue) BIND(C, name='uiSliderSetValue')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: uivalue
  end subroutine uiSliderSetValue

!_UI_EXTERN void uiSliderOnChanged(uiSlider *s, void (*f)(uiSlider *s, void *data), void *data);
  subroutine uiSliderOnChanged(obj, fdata, uidata) BIND(C, name='uiSliderOnChanged')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiSliderOnChanged

!_UI_EXTERN uiSlider *uiNewSlider(int min, int max);
  function uiNewSlider(uimin, uimax) result(ret)  BIND(C, name='uiNewSlider')
    use iso_c_binding
    integer(c_int), value :: uimin, uimax
    type(c_ptr) :: ret
  end function uiNewSlider

!_UI_EXTERN int uiProgressBarValue(uiProgressBar *p);
  function uiProgressBarValue(obj) result(ret) BIND(C, name='uiProgressBarValue')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiProgressBarValue

!_UI_EXTERN void uiProgressBarSetValue(uiProgressBar *p, int n);
  subroutine uiProgressBarSetValue(obj, uivalue) BIND(C, name='uiProgressBarSetValue')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: uivalue
  end subroutine uiProgressBarSetValue

!_UI_EXTERN uiProgressBar *uiNewProgressBar(void);
  function uiNewProgressBar() result(ret)  BIND(C, name='uiNewProgressBar')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewProgressBar

!_UI_EXTERN uiSeparator *uiNewHorizontalSeparator(void);
  function uiNewHorizontalSeparator() result(ret)  BIND(C, name='uiNewHorizontalSeparator')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewHorizontalSeparator

!_UI_EXTERN uiSeparator *uiNewVerticalSeparator(void);
  function uiNewVerticalSeparator() result(ret)  BIND(C, name='uiNewVerticalSeparator')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewVerticalSeparator

!_UI_EXTERN void uiComboboxAppend(uiCombobox *c, const char *text);
  subroutine uiComboboxAppend(obj, text) BIND(C, name='uiComboboxAppend')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: text
  end subroutine uiComboboxAppend

!_UI_EXTERN int uiComboboxSelected(uiCombobox *c);
  function uiComboboxSelected(obj) result(ret) BIND(C, name='uiComboboxSelected')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiComboboxSelected

!_UI_EXTERN void uiComboboxSetSelected(uiCombobox *c, int n);
  subroutine uiComboboxSetSelected(obj, n) BIND(C, name='uiComboboxSetSelected')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: n
  end subroutine uiComboboxSetSelected

!_UI_EXTERN void uiComboboxOnSelected(uiCombobox *c, void (*f)(uiCombobox *c, void *data), void *data);
  subroutine uiComboboxOnSelected(obj, fdata, uidata) BIND(C, name='uiComboboxOnSelected')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiComboboxOnSelected

!_UI_EXTERN uiCombobox *uiNewCombobox(void);
  function uiNewCombobox() result(ret)  BIND(C, name='uiNewCombobox')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewCombobox

!_UI_EXTERN void uiEditableComboboxAppend(uiEditableCombobox *c, const char *text);
  subroutine uiEditableComboboxAppend(obj, text) BIND(C, name='uiEditableComboboxAppend')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: text
  end subroutine uiEditableComboboxAppend

!_UI_EXTERN char *uiEditableComboboxText(uiEditableCombobox *c);
  function uiEditableComboboxText(obj) result(ret) BIND(C, name='uiEditableComboboxText')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiEditableComboboxText

!_UI_EXTERN void uiEditableComboboxSetText(uiEditableCombobox *c, const char *text);
  subroutine uiEditableComboboxSetText(obj, text) BIND(C, name='uiEditableComboboxSetText')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: text
  end subroutine uiEditableComboboxSetText

!_UI_EXTERN void uiEditableComboboxOnChanged(uiEditableCombobox *c, void (*f)(uiEditableCombobox *c, void *data), void *data);
  subroutine uiEditableComboboxOnChanged(obj, fdata, uidata) BIND(C, name='uiEditableComboboxOnChanged')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiEditableComboboxOnChanged

!_UI_EXTERN uiEditableCombobox *uiNewEditableCombobox(void);
  function uiNewEditableCombobox() result(ret)  BIND(C, name='uiNewEditableCombobox')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewEditableCombobox

!_UI_EXTERN void uiRadioButtonsAppend(uiRadioButtons *r, const char *text);
  subroutine uiRadioButtonsAppend(obj, text) BIND(C, name='uiRadioButtonsAppend')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: text
  end subroutine uiRadioButtonsAppend

!_UI_EXTERN int uiRadioButtonsSelected(uiRadioButtons *r);
  function uiRadioButtonsSelected(obj) result(ret) BIND(C, name='uiRadioButtonsSelected')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiRadioButtonsSelected

!_UI_EXTERN void uiRadioButtonsSetSelected(uiRadioButtons *r, int n);
  subroutine uiRadioButtonsSetSelected(obj, n) BIND(C, name='uiRadioButtonsSetSelected')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: n
  end subroutine uiRadioButtonsSetSelected

!_UI_EXTERN void uiRadioButtonsOnSelected(uiRadioButtons *r, void (*f)(uiRadioButtons *, void *), void *data);
  subroutine uiRadioButtonsOnSelected(obj, fdata, uidata) BIND(C, name='uiRadioButtonsOnSelected')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiRadioButtonsOnSelected

!_UI_EXTERN uiRadioButtons *uiNewRadioButtons(void);
  function uiNewRadioButtons() result(ret)  BIND(C, name='uiNewRadioButtons')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewRadioButtons

!_UI_EXTERN uiDateTimePicker *uiNewDateTimePicker(void);
  function uiNewDateTimePicker() result(ret)  BIND(C, name='uiNewDateTimePicker')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewDateTimePicker

!_UI_EXTERN uiDateTimePicker *uiNewDatePicker(void);
  function uiNewDatePicker() result(ret)  BIND(C, name='uiNewDatePicker')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewDatePicker

!_UI_EXTERN uiDateTimePicker *uiNewTimePicker(void);
  function uiNewTimePicker() result(ret)  BIND(C, name='uiNewTimePicker')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewTimePicker

!_UI_EXTERN char *uiMultilineEntryText(uiMultilineEntry *e);
  function uiMultilineEntryText(obj) result(ret) BIND(C, name='uiMultilineEntryText')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiMultilineEntryText

!_UI_EXTERN void uiMultilineEntrySetText(uiMultilineEntry *e, const char *text);
  subroutine uiMultilineEntrySetText(obj, text) BIND(C, name='uiMultilineEntrySetText')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: text
  end subroutine uiMultilineEntrySetText

!_UI_EXTERN void uiMultilineEntryAppend(uiMultilineEntry *e, const char *text);
  subroutine uiMultilineEntryAppend(obj, text) BIND(C, name='uiMultilineEntryAppend')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: text
  end subroutine uiMultilineEntryAppend

!_UI_EXTERN void uiMultilineEntryOnChanged(uiMultilineEntry *e, void (*f)(uiMultilineEntry *e, void *data), void *data);
  subroutine uiMultilineEntryOnChanged(obj, fdata, uidata) BIND(C, name='uiMultilineEntryOnChanged')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiMultilineEntryOnChanged

!_UI_EXTERN int uiMultilineEntryReadOnly(uiMultilineEntry *e);
  function uiMultilineEntryReadOnly(obj) result(ret) BIND(C, name='uiMultilineEntryReadOnly')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiMultilineEntryReadOnly

!_UI_EXTERN void uiMultilineEntrySetReadOnly(uiMultilineEntry *e, int readonly);
  subroutine uiMultilineEntrySetReadOnly(obj, readonly) BIND(C, name='uiMultilineEntrySetReadOnly')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: readonly
  end subroutine uiMultilineEntrySetReadOnly

!_UI_EXTERN uiMultilineEntry *uiNewMultilineEntry(void);
  function uiNewMultilineEntry() result(ret)  BIND(C, name='uiNewMultilineEntry')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewMultilineEntry

!_UI_EXTERN uiMultilineEntry *uiNewNonWrappingMultilineEntry(void);
  function uiNewNonWrappingMultilineEntry() result(ret)  BIND(C, name='uiNewNonWrappingMultilineEntry')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewNonWrappingMultilineEntry

!_UI_EXTERN void uiMenuItemEnable(uiMenuItem *m);
  subroutine uiMenuItemEnable(obj) BIND(C, name='uiMenuItemEnable')
    use iso_c_binding
    type(c_ptr), value :: obj
  end subroutine uiMenuItemEnable

!_UI_EXTERN void uiMenuItemDisable(uiMenuItem *m);
  subroutine uiMenuItemDisable(obj) BIND(C, name='uiMenuItemDisable')
    use iso_c_binding
    type(c_ptr), value :: obj
  end subroutine uiMenuItemDisable

!_UI_EXTERN void uiMenuItemOnClicked(uiMenuItem *m, void (*f)(uiMenuItem *sender, uiWindow *window, void *data), void *data);
  subroutine uiMenuItemOnClicked(obj, fdata, uidata) BIND(C, name='uiMenuItemOnClicked')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiMenuItemOnClicked

!_UI_EXTERN int uiMenuItemChecked(uiMenuItem *m);
  function uiMenuItemChecked(obj) result(ret) BIND(C, name='uiMenuItemChecked')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiMenuItemChecked

!_UI_EXTERN void uiMenuItemSetChecked(uiMenuItem *m, int checked);
  subroutine uiMenuItemSetChecked(obj, checked) BIND(C, name='uiMenuItemSetChecked')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: checked
  end subroutine uiMenuItemSetChecked

!_UI_EXTERN uiMenuItem *uiMenuAppendItem(uiMenu *m, const char *name);
  function uiMenuAppendItem(obj, uiname) result(ret) BIND(C, name='uiMenuAppendItem')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: uiname
    type(c_ptr) :: ret
  end function uiMenuAppendItem

!_UI_EXTERN uiMenuItem *uiMenuAppendCheckItem(uiMenu *m, const char *name);
  function uiMenuAppendCheckItem(obj, uiname) result(ret) BIND(C, name='uiMenuAppendCheckItem')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: uiname
    type(c_ptr) :: ret
  end function uiMenuAppendCheckItem

!_UI_EXTERN uiMenuItem *uiMenuAppendQuitItem(uiMenu *m);
  function uiMenuAppendQuitItem(obj) result(ret) BIND(C, name='uiMenuAppendQuitItem')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiMenuAppendQuitItem

!_UI_EXTERN uiMenuItem *uiMenuAppendPreferencesItem(uiMenu *m);
  function uiMenuAppendPreferencesItem(obj) result(ret) BIND(C, name='uiMenuAppendPreferencesItem')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiMenuAppendPreferencesItem

!_UI_EXTERN uiMenuItem *uiMenuAppendAboutItem(uiMenu *m);
  function uiMenuAppendAboutItem(obj) result(ret) BIND(C, name='uiMenuAppendAboutItem')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiMenuAppendAboutItem

!_UI_EXTERN void uiMenuAppendSeparator(uiMenu *m);
  subroutine uiMenuAppendSeparator(obj) BIND(C, name='uiMenuAppendSeparator')
    use iso_c_binding
    type(c_ptr), value :: obj
  end subroutine uiMenuAppendSeparator

!_UI_EXTERN uiMenu *uiNewMenu(const char *name);
  function uiNewMenu(uiname) result(ret) BIND(C, name='uiNewMenu')
    use iso_c_binding
    character(c_char), dimension(*), intent(in) :: uiname
    type(c_ptr) :: ret
  end function uiNewMenu


!_UI_EXTERN char *uiOpenFile(uiWindow *parent);
  function uiOpenFile(obj) result(ret) BIND(C, name='uiOpenFile')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiOpenFile

!_UI_EXTERN char *uiSaveFile(uiWindow *parent);
  function uiSaveFile(obj) result(ret) BIND(C, name='uiSaveFile')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiSaveFile

!_UI_EXTERN void uiMsgBox(uiWindow *parent, const char *title, const char *description);
  subroutine uiMsgBox(obj, title, description) BIND(C, name='uiMsgBox')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: title
    character(c_char), dimension(*), intent(in) :: description
  end subroutine uiMsgBox

!_UI_EXTERN void uiMsgBoxError(uiWindow *parent, const char *title, const char *description);
  subroutine uiMsgBoxError(obj, title, description) BIND(C, name='uiMsgBoxError')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: title
    character(c_char), dimension(*), intent(in) :: description
  end subroutine uiMsgBoxError

!_UI_EXTERN void uiAreaSetSize(uiArea *a, int width, int height);
  subroutine uiAreaSetSize(a, width, height) BIND(C, name='uiAreaSetSize')
    use iso_c_binding
    type(c_ptr), value :: a
    integer(c_int), value :: width
    integer(c_int), value :: height
  end subroutine uiAreaSetSize

!_UI_EXTERN void uiAreaQueueRedrawAll(uiArea *a);
  subroutine uiAreaQueueRedrawAll(a) BIND(C, name='uiAreaQueueRedrawAll')
    use iso_c_binding
    type(c_ptr), value :: a
  end subroutine uiAreaQueueRedrawAll

!_UI_EXTERN void uiAreaScrollTo(uiArea *a, double x, double y, double width, double height);
  subroutine uiAreaScrollTo(a, x, y, width, height) BIND(C, name='uiAreaScrollTo')
    use iso_c_binding
    type(c_ptr), value :: a
    real(c_double), value :: x, y, width, height
  end subroutine uiAreaScrollTo

!_UI_EXTERN void uiAreaBeginUserWindowMove(uiArea *a);
  subroutine uiAreaBeginUserWindowMove(a) BIND(C, name='uiAreaBeginUserWindowMove')
    use iso_c_binding
    type(c_ptr), value :: a
  end subroutine uiAreaBeginUserWindowMove

!_UI_EXTERN void uiAreaBeginUserWindowResize(uiArea *a, uiWindowResizeEdge edge);
  subroutine uiAreaBeginUserWindowResize(a, edge) BIND(C, name='uiAreaBeginUserWindowResize')
    use iso_c_binding
    type(c_ptr), value :: a
    integer(c_int) :: edge
  end subroutine uiAreaBeginUserWindowResize

!_UI_EXTERN uiArea *uiNewArea(uiAreaHandler *ah);
  function uiNewArea(ah) result(ret)  BIND(C, name='uiNewArea')
    use iso_c_binding
    use libuif_structs
    type(uiAreaHandler) :: ah
    type(c_ptr) :: ret
  end function uiNewArea

!_UI_EXTERN uiArea *uiNewScrollingArea(uiAreaHandler *ah, int width, int height);
  function uiNewScrollingArea(ah, width, height) result(ret)  BIND(C, name='uiNewScrollingArea')
    use iso_c_binding
    use libuif_structs
      type(uiAreaHandler), value :: ah
      integer(c_int), value :: width
      integer(c_int), value :: height
      type(c_ptr) :: ret
  end function uiNewScrollingArea

!_UI_EXTERN uiDrawPath *uiDrawNewPath(uiDrawFillMode fillMode);
  function uiDrawNewPath(fillMode) result(ret)  BIND(C, name='uiDrawNewPath')
    use iso_c_binding
    integer(c_int), value :: fillMode
    type(c_ptr) :: ret
  end function uiDrawNewPath

!_UI_EXTERN void uiDrawFreePath(uiDrawPath *p);
  subroutine uiDrawFreePath(p) BIND(C, name='uiDrawFreePath')
    use iso_c_binding
      type(c_ptr),value :: p
  end subroutine uiDrawFreePath

!_UI_EXTERN void uiDrawPathNewFigure(uiDrawPath *p, double x, double y);
  subroutine uiDrawPathNewFigure(p, x, y) BIND(C, name='uiDrawPathNewFigure')
    use iso_c_binding
    type(c_ptr), value :: p
    real(c_double), value :: x, y
  end subroutine uiDrawPathNewFigure

!_UI_EXTERN void uiDrawPathNewFigureWithArc(uiDrawPath *p, double xCenter, double yCenter, double radius, double startAngle, double sweep, int negative);
  subroutine uiDrawPathNewFigureWithArc(p,xCenter,yCenter,radius,startAngle,sweep,negative) &
& BIND(C, name='uiDrawPathNewFigureWithArc')
    use iso_c_binding
    type(c_ptr), value :: p
    real(c_double), value :: xCenter, yCenter, radius, startAngle, sweep
    integer(c_int), value :: negative
  end subroutine uiDrawPathNewFigureWithArc

!_UI_EXTERN void uiDrawPathLineTo(uiDrawPath *p, double x, double y);
  subroutine uiDrawPathLineTo(p, x, y) BIND(C, name='uiDrawPathLineTo')
    use iso_c_binding
    type(c_ptr), value :: p
    real(c_double), value :: x, y
  end subroutine uiDrawPathLineTo

!_UI_EXTERN void uiDrawPathArcTo(uiDrawPath *p, double xCenter, double yCenter, double radius, double startAngle, double sweep, int negative);
  subroutine uiDrawPathArcTo(p,xCenter,yCenter,radius,startAngle,sweep,negative) BIND(C, name='uiDrawPathArcTo')
    use iso_c_binding
    type(c_ptr), value :: p
    real(c_double), value :: xCenter, yCenter, radius, startAngle, sweep
    integer(c_int), value :: negative
  end subroutine uiDrawPathArcTo

!_UI_EXTERN void uiDrawPathBezierTo(uiDrawPath *p, double c1x, double c1y, double c2x, double c2y, double endX, double endY);
  subroutine uiDrawPathBezierTo(p,c1x,c1y,c2x,c2y,endX,endY) BIND(C, name='uiDrawPathBezierTo')
    use iso_c_binding
    type(c_ptr), value :: p
    real(c_double), value :: c1x,c1y,c2x,c2y,endX,endY
  end subroutine uiDrawPathBezierTo

!_UI_EXTERN void uiDrawPathCloseFigure(uiDrawPath *p);
  subroutine uiDrawPathCloseFigure(p) BIND(C, name='uiDrawPathCloseFigure')
    use iso_c_binding
    type(c_ptr), value :: p
  end subroutine uiDrawPathCloseFigure

!_UI_EXTERN void uiDrawPathAddRectangle(uiDrawPath *p, double x, double y, double width, double height);
  subroutine uiDrawPathAddRectangle(p,x, y, width, height) BIND(C, name='uiDrawPathAddRectangle')
    use iso_c_binding
      type(c_ptr),value :: p
      real(c_double), value :: x, y, width, height
  end subroutine uiDrawPathAddRectangle

!_UI_EXTERN void uiDrawPathEnd(uiDrawPath *p);
  subroutine uiDrawPathEnd(p) BIND(C, name='uiDrawPathEnd')
    use iso_c_binding
      type(c_ptr), value :: p
  end subroutine uiDrawPathEnd

!_UI_EXTERN void uiDrawStroke(uiDrawContext *c, uiDrawPath *path, uiDrawBrush *b, uiDrawStrokeParams *p);
  subroutine uiDrawStroke(c, path, b, p) BIND(C, name='uiDrawStroke')
    use iso_c_binding
    use libuif_structs
      type(c_ptr), value :: c
      type(c_ptr), value :: path
      type(uiDrawBrush) :: b
      type(uiDrawStrokeParams) :: p
  end subroutine uiDrawStroke

!_UI_EXTERN void uiDrawFill(uiDrawContext *c, uiDrawPath *path, uiDrawBrush *b);
  subroutine uiDrawFill(c, path, b) BIND(C, name='uiDrawFill')
    use iso_c_binding
    use libuif_structs
      type(c_ptr), value :: c
      type(c_ptr), value :: path
      type(uiDrawBrush) :: b
  end subroutine uiDrawFill

!_UI_EXTERN void uiDrawMatrixSetIdentity(uiDrawMatrix *m);
  subroutine uiDrawMatrixSetIdentity(m) BIND(C, name='uiDrawMatrixSetIdentity')
    use iso_c_binding
    use libuif_structs
      type(uiDrawMatrix) :: m
  end subroutine uiDrawMatrixSetIdentity

!_UI_EXTERN void uiDrawMatrixTranslate(uiDrawMatrix *m, double x, double y);
  subroutine uiDrawMatrixTranslate(m, x, y) BIND(C, name='uiDrawMatrixTranslate')
    use iso_c_binding
    use libuif_structs
      type(uiDrawMatrix) :: m
      real(c_double), value :: x, y
  end subroutine uiDrawMatrixTranslate

!_UI_EXTERN void uiDrawMatrixScale(uiDrawMatrix *m, double xCenter, double yCenter, double x, double y);
  subroutine uiDrawMatrixScale(m, xCenter, yCenter, x, y) BIND(C, name='uiDrawMatrixScale')
    use iso_c_binding
    use libuif_structs
      type(uiDrawMatrix) :: m
      real(c_double), value :: xCenter, yCenter, x, y
  end subroutine uiDrawMatrixScale

!_UI_EXTERN void uiDrawMatrixRotate(uiDrawMatrix *m, double x, double y, double amount);
  subroutine uiDrawMatrixRotate(m, x, y, amount) BIND(C, name='uiDrawMatrixRotate')
    use iso_c_binding
    use libuif_structs
      type(uiDrawMatrix) :: m
      real(c_double), value :: x, y, amount
  end subroutine uiDrawMatrixRotate

!_UI_EXTERN void uiDrawMatrixSkew(uiDrawMatrix *m, double x, double y, double xamount, double yamount);
  subroutine uiDrawMatrixSkew(m, x, y, xamount, yamount) BIND(C, name='uiDrawMatrixSkew')
    use iso_c_binding
    use libuif_structs
      type(uiDrawMatrix) :: m
      real(c_double), value :: x, y, xamount, yamount
  end subroutine uiDrawMatrixSkew

!_UI_EXTERN void uiDrawMatrixMultiply(uiDrawMatrix *dest, uiDrawMatrix *src);
  subroutine uiDrawMatrixMultiply(m, src) BIND(C, name='uiDrawMatrixMultiply')
    use iso_c_binding
    use libuif_structs
      type(uiDrawMatrix) :: m
      type(c_ptr), value :: src
  end subroutine uiDrawMatrixMultiply

!_UI_EXTERN int uiDrawMatrixInvertible(uiDrawMatrix *m);
  function uiDrawMatrixInvertible(m) result(ret)  BIND(C, name='uiDrawMatrixInvertible')
    use iso_c_binding
    use libuif_structs
      type(uiDrawMatrix) :: m
      integer(c_int) :: ret
  end function uiDrawMatrixInvertible

!_UI_EXTERN int uiDrawMatrixInvert(uiDrawMatrix *m);
  function uiDrawMatrixInvert(m) result(ret)  BIND(C, name='uiDrawMatrixInvert')
    use iso_c_binding
    use libuif_structs
      type(uiDrawMatrix) :: m
      integer(c_int) :: ret
  end function uiDrawMatrixInvert

!_UI_EXTERN void uiDrawMatrixTransformPoint(uiDrawMatrix *m, double *x, double *y);
  subroutine uiDrawMatrixTransformPoint(m, x, y) BIND(C, name='uiDrawMatrixTransformPoint')
    use iso_c_binding
    use libuif_structs
      type(uiDrawMatrix) :: m
      real(c_double), value :: x, y
  end subroutine uiDrawMatrixTransformPoint

!_UI_EXTERN void uiDrawMatrixTransformSize(uiDrawMatrix *m, double *x, double *y);
  subroutine uiDrawMatrixTransformSize(m, x, y) BIND(C, name='uiDrawMatrixTransformSize')
    use iso_c_binding
    use libuif_structs
      type(uiDrawMatrix) :: m
      real(c_double), value :: x, y
  end subroutine uiDrawMatrixTransformSize

!_UI_EXTERN void uiDrawTransform(uiDrawContext *c, uiDrawMatrix *m);
  subroutine uiDrawTransform(c, m) BIND(C, name='uiDrawTransform')
    use iso_c_binding
    use libuif_structs
      type(c_ptr), value :: c
      type(uiDrawMatrix) :: m
  end subroutine uiDrawTransform

!_UI_EXTERN void uiDrawClip(uiDrawContext *c, uiDrawPath *path);
  subroutine uiDrawClip(c, path) BIND(C, name='uiDrawClip')
    use iso_c_binding
    type(c_ptr), value :: c
    type(c_ptr), value :: path
  end subroutine uiDrawClip

!_UI_EXTERN void uiDrawSave(uiDrawContext *c);
  subroutine uiDrawSave(c) BIND(C, name='uiDrawSave')
    use iso_c_binding
    type(c_ptr), value :: c
  end subroutine uiDrawSave

!_UI_EXTERN void uiDrawRestore(uiDrawContext *c);
  subroutine uiDrawRestore(c) BIND(C, name='uiDrawRestore')
    use iso_c_binding
    type(c_ptr), value :: c
  end subroutine uiDrawRestore


!_UI_EXTERN uiDrawFontFamilies *uiDrawListFontFamilies(void);
  function uiDrawListFontFamilies() result(ret)  BIND(C, name='uiDrawListFontFamilies')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiDrawListFontFamilies

!_UI_EXTERN int uiDrawFontFamiliesNumFamilies(uiDrawFontFamilies *ff);
  function uiDrawFontFamiliesNumFamilies(obj) result(ret)  BIND(C, name='uiDrawFontFamiliesNumFamilies')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiDrawFontFamiliesNumFamilies

!_UI_EXTERN char *uiDrawFontFamiliesFamily(uiDrawFontFamilies *ff, int n);
  function uiDrawFontFamiliesFamily(obj, n) result(ret) BIND(C, name='uiDrawFontFamiliesFamily')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: n
    type(c_ptr) :: ret
  end function uiDrawFontFamiliesFamily

!_UI_EXTERN void uiDrawFreeFontFamilies(uiDrawFontFamilies *ff);
  subroutine uiDrawFreeFontFamilies(obj) BIND(C, name='uiDrawFreeFontFamilies')
    use iso_c_binding
    type(c_ptr), value :: obj
  end subroutine uiDrawFreeFontFamilies

!_UI_EXTERN uiDrawTextFont *uiDrawLoadClosestFont(const uiDrawTextFontDescriptor *desc);
  function uiDrawLoadClosestFont(desc) result(ret) BIND(C, name='uiDrawLoadClosestFont')
    use iso_c_binding
    type(c_ptr), value :: desc
    type(c_ptr) :: ret
  end function uiDrawLoadClosestFont

!_UI_EXTERN void uiDrawFreeTextFont(uiDrawTextFont *font);
  subroutine uiDrawFreeTextFont(font) BIND(C, name='uiDrawFreeTextFont')
    use iso_c_binding
    type(c_ptr), value :: font
  end subroutine uiDrawFreeTextFont

!_UI_EXTERN uintptr_t uiDrawTextFontHandle(uiDrawTextFont *font);
  function uiDrawTextFontHandle(font) result(ret) BIND(C, name='uiDrawTextFontHandle')
    use iso_c_binding
    type(c_ptr), value :: font
    integer(c_int), pointer :: ret
  end function uiDrawTextFontHandle

!_UI_EXTERN void uiDrawTextFontDescribe(uiDrawTextFont *font, uiDrawTextFontDescriptor *desc);
  subroutine uiDrawTextFontDescribe(font, desc) BIND(C, name='uiDrawTextFontDescribe')
    use iso_c_binding
    type(c_ptr), value :: font, desc
  end subroutine uiDrawTextFontDescribe

!_UI_EXTERN void uiDrawTextFontGetMetrics(uiDrawTextFont *font, uiDrawTextFontMetrics *metrics);
  subroutine uiDrawTextFontGetMetrics(font, metrics) BIND(C, name='uiDrawTextFontGetMetrics')
    use iso_c_binding
    type(c_ptr), value :: font, metrics
  end subroutine uiDrawTextFontGetMetrics

!_UI_EXTERN uiDrawTextLayout *uiDrawNewTextLayout(const char *text, uiDrawTextFont *defaultFont, double width);
  function uiDrawNewTextLayout(text, defaultFont, width) result(ret) BIND(C, name='uiDrawNewTextLayout')
    use iso_c_binding
    character(c_char), dimension(*), intent(in) :: text
    type(c_ptr),value :: defaultFont
    real(c_double), value :: width
    type(c_ptr) :: ret
  end function uiDrawNewTextLayout

!_UI_EXTERN void uiDrawFreeTextLayout(uiDrawTextLayout *layout);
  subroutine uiDrawFreeTextLayout(layout) BIND(C, name='uiDrawFreeTextLayout')
    use iso_c_binding
    type(c_ptr), value :: layout
  end subroutine uiDrawFreeTextLayout

!_UI_EXTERN void uiDrawTextLayoutSetWidth(uiDrawTextLayout *layout, double width);
  subroutine uiDrawTextLayoutSetWidth(layout, width) BIND(C, name='uiDrawTextLayoutSetWidth')
    use iso_c_binding
    type(c_ptr), value :: layout
    real(c_double), value :: width
  end subroutine uiDrawTextLayoutSetWidth

!_UI_EXTERN void uiDrawTextLayoutExtents(uiDrawTextLayout *layout, double *width, double *height);
  subroutine uiDrawTextLayoutExtents(layout, width, height) BIND(C, name='uiDrawTextLayoutExtents')
    use iso_c_binding
    type(c_ptr), value :: layout
    real(c_double) :: width, height
  end subroutine uiDrawTextLayoutExtents

!_UI_EXTERN void uiDrawTextLayoutSetColor(uiDrawTextLayout *layout, int startChar, int endChar, double r, double g, double b, double a);
  subroutine uiDrawTextLayoutSetColor(layout, startChar, endChar, r, g, b, a) BIND(C, name='uiDrawTextLayoutSetColor')
    use iso_c_binding
    type(c_ptr), value :: layout
    integer(c_int), value :: startChar, endChar
    real(c_double), value :: r, g, b, a
  end subroutine uiDrawTextLayoutSetColor

!_UI_EXTERN void uiDrawText(uiDrawContext *c, double x, double y, uiDrawTextLayout *layout);
  subroutine uiDrawText(obj, x, y, layout) BIND(C, name='uiDrawText')
    use iso_c_binding
    type(c_ptr), value :: obj
    real(c_double), value :: x, y
    type(c_ptr), value :: layout
  end subroutine uiDrawText

!_UI_EXTERN uiDrawTextFont *uiFontButtonFont(uiFontButton *b);
  function uiFontButtonFont(obj) result(ret) BIND(C, name='uiFontButtonFont')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr) :: ret
  end function uiFontButtonFont

!_UI_EXTERN void uiFontButtonOnChanged(uiFontButton *b, void (*f)(uiFontButton *, void *), void *data);
  subroutine uiFontButtonOnChanged(obj, fdata, uidata) BIND(C, name='uiFontButtonOnChanged')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiFontButtonOnChanged

!_UI_EXTERN uiFontButton *uiNewFontButton(void);
  function uiNewFontButton() result(ret) BIND(C, name='uiNewFontButton')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewFontButton

!_UI_EXTERN void uiColorButtonColor(uiColorButton *b, double *r, double *g, double *bl, double *a);
  subroutine uiColorButtonColor(b, r, g ,bl, a) BIND(C, name='uiColorButtonColor')
    use iso_c_binding
    type(c_ptr), value :: b
    real(c_double) :: r, g ,bl, a
  end subroutine uiColorButtonColor

!_UI_EXTERN void uiColorButtonSetColor(uiColorButton *b, double r, double g, double bl, double a);
  subroutine uiColorButtonSetColor(b, r, g ,bl, a) BIND(C, name='uiColorButtonSetColor')
    use iso_c_binding
    type(c_ptr), value :: b
    real(c_double) :: r, g ,bl, a
  end subroutine uiColorButtonSetColor

!_UI_EXTERN void uiColorButtonOnChanged(uiColorButton *b, void (*f)(uiColorButton *, void *), void *data);
  subroutine uiColorButtonOnChanged(b, fdata, uidata) BIND(C, name='uiColorButtonOnChanged')
    use iso_c_binding
    type(c_ptr), value :: b
    type(c_funptr), value :: fdata
    type(c_ptr), value :: uidata
  end subroutine uiColorButtonOnChanged

!_UI_EXTERN uiColorButton *uiNewColorButton(void);
  function uiNewColorButton() result(ret) BIND(C, name='uiNewColorButton')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewColorButton

!_UI_EXTERN void uiFormAppend(uiForm *f, const char *label, uiControl *c, int stretchy);
  subroutine uiFormAppend(obj, label, obj1, stretchy) BIND(C, name='uiFormAppend')
    use iso_c_binding
    type(c_ptr), value :: obj
    character(c_char), dimension(*), intent(in) :: label
    type(c_ptr), value :: obj1
    integer(c_int), value :: stretchy
  end subroutine uiFormAppend

!_UI_EXTERN void uiFormDelete(uiForm *f, int index);
  subroutine uiFormDelete(obj, uiindex) BIND(C, name='uiFormDelete')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: uiindex
  end subroutine uiFormDelete

!_UI_EXTERN int uiFormPadded(uiForm *f);
  function uiFormPadded(obj) result(ret) BIND(C, name='uiFormPadded')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiFormPadded

!_UI_EXTERN void uiFormSetPadded(uiForm *f, int padded);
  subroutine uiFormSetPadded(obj, padded) BIND(C, name='uiFormSetPadded')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: padded
  end subroutine uiFormSetPadded

!_UI_EXTERN uiForm *uiNewForm(void);
  function uiNewForm() result(ret) BIND(C, name='uiNewForm')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewForm

!_UI_EXTERN void uiGridAppend(uiGrid *g, uiControl *c, int left, int top, int xspan, int yspan, int hexpand, uiAlign halign, int vexpand, uiAlign valign);
  subroutine uiGridAppend(obj, obj1, left, top, xspan, yspan, hexpand, halign, vexpand, valign) BIND(C, name='uiGridAppend')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr), value :: obj1
    integer(c_int), value :: left, top, xspan, yspan, hexpand, halign, vexpand, valign
  end subroutine uiGridAppend

!_UI_EXTERN void uiGridInsertAt(uiGrid *g, uiControl *c, uiControl *existing, uiAt at, int xspan, int yspan, int hexpand, uiAlign halign, int vexpand, uiAlign valign);
  subroutine uiGridInsertAt(obj, obj1, obj2, at, xspan, yspan, hexpand, halign, vexpand, valign) BIND(C, name='uiGridInsertAt')
    use iso_c_binding
    type(c_ptr), value :: obj
    type(c_ptr), value :: obj1, obj2
    integer(c_int), value :: at, xspan, yspan, hexpand, halign, vexpand, valign
  end subroutine uiGridInsertAt

!_UI_EXTERN int uiGridPadded(uiGrid *g);
  function uiGridPadded(obj) result(ret) BIND(C, name='uiGridPadded')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int) :: ret
  end function uiGridPadded

!_UI_EXTERN void uiGridSetPadded(uiGrid *g, int padded);
  subroutine uiGridSetPadded(obj, padded) BIND(C, name='uiGridSetPadded')
    use iso_c_binding
    type(c_ptr), value :: obj
    integer(c_int), value :: padded
  end subroutine uiGridSetPadded

!_UI_EXTERN uiGrid *uiNewGrid(void);
  function uiNewGrid() result(ret) BIND(C, name='uiNewGrid')
    use iso_c_binding
    type(c_ptr) :: ret
  end function uiNewGrid

end interface

end module libuif

