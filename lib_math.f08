module lib_math

    use lib_kinds, only: sp, dp

    implicit none

    private
    public :: trapz

    interface trapz
        module procedure trapz_xy_sp
        module procedure trapz_xy_dp
        module procedure trapz_y_sp
        module procedure trapz_y_dp
        module procedure trapz_func_sp
        module procedure trapz_func_dp
    end interface

    interface trapezium
        module procedure trapezium_sp
        module procedure trapezium_dp
    end interface

contains

    pure function trapz_xy_sp(x, y) result(s)
        real(sp), intent(in) :: x(:), y(:)
        integer :: i
        real(sp) :: s

        s = 0.0_sp

        do i = 1, size(y) - 1
            s = s + trapezium(x(i), x(i + 1), y(i), y(i + 1))
        end do

    end function

    pure function trapz_xy_dp(x, y) result(s)
        real(dp), intent(in) :: x(:), y(:)
        integer :: i
        real(dp) :: s

        s = 0.0_dp

        do i = 1, size(y) - 1
            s = s + trapezium(x(i), x(i + 1), y(i), y(i + 1))
        end do

    end function

    pure function trapz_y_sp(y) result(s)
        real(sp), intent(in) :: y(:)
        integer :: i
        real(sp) :: s

        s = 0.0_sp

        do i = 1, size(y) - 1
            s = s + trapezium(0.0_sp, 1.0_sp, y(i), y(i + 1))
        end do

    end function

    pure function trapz_y_dp(y) result(s)
        real(dp), intent(in) :: y(:)
        integer :: i
        real(dp) :: s

        s = 0.0_dp

        do i = 1, size(y) - 1
            s = s + trapezium(0.0_dp, 1.0_dp, y(i), y(i + 1))
        end do

    end function

    pure function trapz_func_sp(f, a, b, n) result(s)
        interface
            pure function f(x)
                use lib_kinds, only: sp
                real(sp), intent(in) :: x
                real(sp) :: f
            end function
        end interface
        real(sp), intent(in) :: a, b
        integer, intent(in) :: n
        integer :: i
        real(sp) :: s, dx, x1, x2, y1, y2

        s = 0.0_sp
        dx = (b - a) / real(n, kind=sp)

        do i = 1, n
            x1 = a + real((i - 1), kind=sp) * dx
            x2 = x1 + dx
            y1 = f(x1)
            y2 = f(x2)
            s = s + trapezium(x1, x2, y1, y2)
        end do

    end function

    pure function trapz_func_dp(f, a, b, n) result(s)
        interface
            pure function f(x)
                use lib_kinds, only: dp
                real(dp), intent(in) :: x
                real(dp) :: f
            end function
        end interface
        real(dp), intent(in) :: a, b
        integer, intent(in) :: n
        integer :: i
        real(dp) :: s, dx, x1, x2, y1, y2

        s = 0.0_dp
        dx = (b - a) / real(n, kind=dp)

        do i = 1, n
            x1 = a + real((i - 1), kind=dp) * dx
            x2 = x1 + dx
            y1 = f(x1)
            y2 = f(x2)
            s = s + trapezium(x1, x2, y1, y2)
        end do

    end function

    pure function trapezium_sp(x1, x2, y1, y2) result(s)
        real(sp), intent(in) :: x1, x2, y1, y2
        real(sp) :: s
        s = 0.5_sp * (y1 + y2) * (x2 - x1)
    end function

    pure function trapezium_dp(x1, x2, y1, y2) result(s)
        real(dp), intent(in) :: x1, x2, y1, y2
        real(dp) :: s
        s = 0.5_dp * (y1 + y2) * (x2 - x1)
    end function

end module

program main_lib_math
    use lib_math
    use lib_array, only: linspace
    use lib_kinds, only: sp, dp
    implicit none

    real(sp), allocatable :: x_sp(:), y_sp(:)
    real(dp), allocatable :: x_dp(:), y_dp(:)
    real(dp) :: t1, t2
    integer :: i, n

    call cpu_time(t1)
    do n = 1, 10000

        allocate (x_sp(n), x_dp(n), y_sp(n), y_dp(n))

        x_sp = linspace(0.0_sp, 1.0_sp, n)
        x_dp = linspace(0.0_dp, 1.0_dp, n)

        do i = 1, n
            y_sp(i) = f_sp(x_sp(i))
            y_dp(i) = f_dp(x_dp(i))
        end do

        ! print *, &
        !     trapz(x_sp, y_sp), &
        !     trapz(x_dp, y_dp), &
        !     trapz(f_sp, -0.0_sp, 1.0_sp, n - 1), &
        !     trapz(f_dp, -0.0_dp, 1.0_dp, n - 1)

        deallocate (x_sp, x_dp, y_sp, y_dp)
    end do
    call cpu_time(t2)
    ! print *, 'Time: ', (t2 - t1), ' sec'

contains
    pure function f_sp(x)
        real(sp), intent(in) :: x
        real(sp) :: f_sp
        f_sp = sin(x)
    end function

    pure function f_dp(x)
        real(dp), intent(in) :: x
        real(dp) :: f_dp
        f_dp = sin(x)
    end function

end program

