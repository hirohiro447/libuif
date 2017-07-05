!
! libuif_parts.f90
!
! pthread part from
! https://stackoverflow.com/questions/26791589/calling-a-subroutine-in-fortran-without-blocking-the-main-program
!
! This module need link libuif_parts.a (libuif_parts.o & pthreads.o) & pthread (-llibuif_parts.a -lpthread)
!
! subroutine(interface) pthread_create_opaque & pthread_join_opaque requierd 
!                                          pthread.c (object pthreads.o) or libuif_parts.a
!
! function utf8len,utf8lenw requierd libuif_parts.o from libuif_parts.f90 ar in libuif_parts.a
!
! subroutine usleep(interface) requierd link pthread(-lpthread)
!
! Created by hirohiro447
! 2017/6/14
!
module libuif_parts
implicit none

interface

  subroutine pthread_create_opaque(threadptr, procptr, dataptr, err) bind(C,name="pthread_create_opaque")
    use iso_c_binding
    type(c_ptr) :: threadptr
    type(c_funptr),value :: procptr
    type(c_ptr),value :: dataptr
    integer(c_int),intent(out) :: err
  end subroutine

  subroutine pthread_join_opaque(thread, err) bind(C,name="pthread_join_opaque")
    use iso_c_binding
    type(c_ptr),value :: thread
    integer(c_int),intent(out) :: err
  end subroutine

  subroutine usleep(useconds) bind(C)
    use iso_c_binding
    implicit none
    integer(c_int32_t), value :: useconds
  end subroutine usleep

end interface

contains

! count UTF-8 String Length
  function utf8len(string) result(ret)
    implicit none
      character(*), intent(in) :: string
      integer :: i,j,k,size,ret,code
      ret = 0
      k = 1
      do i = 1,len(string),1
        size = 1
        code = ICHAR(string(k:k))
        if (code == 0) then
          exit
        else if (IAND(x'80',code) /= 0) then  ! /* 1バイト文字以外 */
          do j = 2, 8
            code = ISHFT(code,1)
            if (IAND(x'80',code) == 0) then
                exit
            end if
            size = size + 1
          end do
        end if
          ret = ret + 1
          k = k + size
      end do
  end function utf8len

  function utf8lenw(string) result(ret)
    implicit none
      character(*), intent(in) :: string
      integer :: i,j,k,size,ret,code,hkana
      ret = 0
      k = 1
      do i = 1,len(string),1
        size = 1
        code = ICHAR(string(k:k))
        if (code == 0) then
          exit
        else if (IAND(x'80',code) /= 0) then  ! /* 1バイト文字以外 */
          do j = 2, 8
            code = ISHFT(code,1)
            if (IAND(x'80',code) == 0) then
                exit
            end if
            size = size + 1
          end do
        end if
        if ( size == 1) then
          ret = ret + 1
        else if ( ICHAR(string(k:k)) == x'EF') then
          hkana = ICHAR(string(k:k)) * 256 * 256 + ICHAR(string(k+1:k+1)) * 256 + ICHAR(string(k+2:k+2))
! どっちが正しい？？？？
          if ((hkana >= x'EFBDA1' .AND. hkana <= x'EFBDBF') .OR. (hkana >= x'EFBE80' .AND. hkana <= x'EFBE9F')) then 
!          if (hkana >= x'EFBC81' .AND. hkana <= x'EFBEBF') then 
            ret = ret + 1
          else 
            ret = ret + 2
          end if
        else
          ret = ret + 2
        end if
          k = k + size
      end do
  end function utf8lenw

 end module libuif_parts

