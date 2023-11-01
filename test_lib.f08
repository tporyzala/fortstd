program test_lib

    use lib_kinds, only: wp => sp
    use lib_constants
    use lib_array
    use lib_linalg
    use lib_math
    use lib_random
    use lib_statistics

    implicit none

    real(wp) :: empty(0), x, x1, x2, y, y1, y2, t, t1, t2
    ! character(len=1), parameter :: f = '*'

    ! lib_array
    ! public :: linspace, logspace, arrange, diff, unique
    print *, 'LINSPACE'
    print *, repeat('-', 20)
    print *, 3, linspace(0.0_wp, 1.0_wp, 3)
    print *, 2, linspace(0.0_wp, 1.0_wp, 2)
    print *, 1, linspace(0.0_wp, 1.0_wp, 1)
    print *, 0, linspace(0.0_wp, 1.0_wp, 0)

    print *, 'LOGSPACE'
    print *, repeat('-', 20)
    print *, 3, logspace(0.0_wp, 1.0_wp, 3)
    print *, 2, logspace(0.0_wp, 1.0_wp, 2)
    print *, 1, logspace(0.0_wp, 1.0_wp, 1)
    print *, 0, logspace(0.0_wp, 1.0_wp, 0)

    print *, 'ARRANGE'
    print *, repeat('-', 20)
    print *, arrange(1.0_wp, 5.0_wp, 1.0_wp)
    print *, arrange(1.0_wp, 5.0_wp, 2.0_wp)
    print *, arrange(0.0_wp, 5.0_wp, 2.0_wp)
    print *, arrange(0, 5, 1)
    print *, arrange(1, 5, 2)
    print *, arrange(0, 5, 2)

    print *, 'DIFF'
    print *, repeat('-', 20)
    print *, diff(real((/1/), kind=wp))
    print *, diff(real((/1, 2, 3, 5, 8, 1/), kind=wp))
    print *, diff(real(reshape((/1, 2, 5, 0/), (/2, 2/)), kind=wp), 1)
    print *, diff(real(reshape((/1, 2, 5, 0/), (/2, 2/)), kind=wp), 2)

    ! print *, 'UNIQUE'
    ! print *, repeat('-', 20)
    ! print f, unique((/2.0_wp, 2.0_wp, 1.0_wp, 2.0_wp, -1.0_wp, 22.0_wp, 1.0_wp, 3.0_wp/))
    ! print f, unique(reshape((/2.0_wp, 2.0_wp, 1.0_wp, 2.0_wp, -1.0_wp, 22.0_wp, 1.0_wp, 3.0_wp/), (/4, 2/)))

    ! print *, 'MEAN'
    ! print *, repeat('-', 20)
    ! print f, mean((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp/))
    ! print f, mean((/1.0_wp/))
    ! print f, mean(empty)
    ! print f, mean(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)))
    ! print f, mean(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)), 1)
    ! print f, mean(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)), 2)

    ! print *, 'STD'
    ! print *, repeat('-', 20)
    ! print f, std((/1.0_wp, 2.0_wp, 3.0_wp/))
    ! print f, std((/1.0_wp, 1.0_wp, 1.0_wp/))
    ! print f, std((/1.0_wp/))
    ! print f, std(empty)
    ! print f, std(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)))
    ! print f, std(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)), 1)
    ! print f, std(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)), 2)

contains

    pure function f_sin(input) result(output)
        real(wp), intent(in) :: input
        real(wp) :: output
        output = sin(input)
    end function

    pure function f_cos(input) result(output)
        real(wp), intent(in) :: input
        real(wp) :: output
        output = cos(input)
    end function

    pure function f_squared(input) result(output)
        real(wp), intent(in) :: input
        real(wp) :: output
        output = input**2
    end function

end program
