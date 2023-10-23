module myMatLib

    use iso_c_binding

    implicit none

    ! fortran to c header data types
    ! real(c_double)    -->     double
    ! real(c_float)     -->     float
    !
    ! CANNOT PASS ARRAYS

    ! PARALLEL
    ! gfortran -shared -o matlab/myMatLib.dll matlab/myMatLib.f08 -fopenmp -Ofast -ftree-parallelize-loops=6

contains

    pure function test(a) result(b) bind(c, name='test')
        !DEC$ ATTRIBUTES DLLEXPORT :: test
        real(c_double), intent(in) :: a
        real(c_double) :: b
        b = a * a
    end function

    subroutine test2(a, b) bind(c, name='test2')
        !DEC$ ATTRIBUTES DLLEXPORT :: test2
        real(c_double), intent(in) :: a(2, 2)
        real(c_double), intent(inout) :: b(2, 2)
        b = a * a
    end subroutine

    subroutine test3(n, m, a, b) bind(c, name='test3')
        !DEC$ ATTRIBUTES DLLEXPORT :: test3
        real(c_double), intent(in) :: n, m
        real(c_double), intent(in) :: a(int(n), int(m))
        real(c_double), intent(inout) :: b(int(n), int(m))
        integer :: i
        do i = 1, 100000000
            b = a * a
        end do
    end subroutine

end module
