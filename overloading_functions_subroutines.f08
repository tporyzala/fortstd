module overload

    interface check_kind
        module procedure check_int, check_real
    end interface

    interface check_kind_sub
        module procedure check_int_sub, check_real_sub
    end interface

contains

    integer function check_int(a) result(k)
        integer, intent(in) :: a
        k = kind(a)
        print *, 'kind(a) == integer // function'
    end function

    integer function check_real(a) result(k)
        real, intent(in) :: a
        k = kind(a)
        print *, 'kind(a) == real // function'
    end function

    subroutine check_int_sub(a, k)
        integer, intent(in) :: a
        integer, intent(out) :: k
        k = kind(a)
        print *, 'kind(a) == integer // subroutine'
    end subroutine

    subroutine check_real_sub(a, k)
        real, intent(in) :: a
        integer, intent(out) :: k
        k = kind(a)
        print *, 'kind(a) == real // subroutine'
    end subroutine

end module

program main

    use iso_fortran_env
    use overload

    implicit none

    integer :: check_kind, k

    k = check_kind(1)
    k = check_kind(1.0)

    call check_kind_sub(1, k)
    call check_kind_sub(1.0, k)

end program
