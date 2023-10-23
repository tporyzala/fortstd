module check_kind_mod

    use iso_fortran_env
    implicit none

contains

    subroutine sub(x)
        class(*), intent(in) :: x
        select type (x)
        type is (real(real32))
            block
                print *, "type is real32"
            end block
        type is (real(real64))
            block
                print *, "type is real64"
            end block
        type is (integer)
            block
                print *, "type is int"
            end block
        type is (integer(int8))
            block
                print *, "type is int8"
            end block
        type is (integer(int16))
            block
                print *, "type is inter16"
            end block
        end select
    end subroutine sub

end module check_kind_mod
!
program main
    use iso_fortran_env
    use check_kind_mod

    implicit none

    integer :: i
    integer(kind=int8) :: i8
    integer(kind=int16) :: i16

    call sub(1.0)
    call sub(1.0d0)
    
    call sub(i)
    call sub(i8)
    call sub(i16)

end program main
