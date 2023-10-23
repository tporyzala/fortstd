module lib_statistics

    use lib_kinds, only: wp

    implicit none

    private
    public :: mean, std

    interface mean
        module procedure mean_1d, mean_2d, mean_2d_dim
    end interface

    interface std
        module procedure std_1d, std_2d, std_2d_dim
    end interface

contains

    pure function mean_1d(a) result(y)
        real(wp), intent(in) :: a(:)
        real(wp) :: N, y

        N = real(size(a))
        y = sum(a) / max(1.0, N)

    end function

    pure function mean_2d(a) result(y)
        real(wp), intent(in) :: a(:, :)
        real(wp) :: N, y

        N = real(size(a))
        y = sum(a) / max(1.0, N)

    end function

    pure function mean_2d_dim(a, dim) result(y)
        real(wp), intent(in) :: a(:, :)
        integer, intent(in) :: dim
        real(wp) :: N, y(size(a, 3 - dim))

        N = real(size(a, dim))
        y = sum(a, dim) / max(1.0, N)

    end function

    pure function std_1d(a) result(y)
        real(wp), intent(in) :: a(:)
        real(wp) :: N, mu, y

        mu = mean(a)
        N = real(size(a))
        y = sqrt(sum((a - mu)**2) / (max(2.0, N) - 1.0))

    end function

    pure function std_2d(a) result(y)
        real(wp), intent(in) :: a(:, :)
        real(wp) :: N, mu, y

        mu = mean(a)
        N = real(size(a))
        y = sqrt(sum((a - mu)**2) / (max(2.0, N) - 1.0))

    end function

    pure function std_2d_dim(a, dim) result(y)
        real(wp), intent(in) :: a(:, :)
        integer, intent(in) :: dim
        integer :: i, sz
        real(wp) :: N, mu(size(a, 3 - dim))
        real(wp) :: y(size(a, 3 - dim))

        mu = mean(a, dim)
        N = real(size(a, dim))
        sz = size(a, 3 - dim)

        if (dim == 1) then
            do i = 1, sz
                y(i) = sum((a(:, i) - mu(i))**2)
            end do
        else if (dim == 2) then
            do i = 1, sz
                y(i) = sum((a(i, :) - mu(i))**2)
            end do
        end if
        y = sqrt(y / (max(2.0, N) - 1.0))

    end function

end module

program main

    use lib_kinds, only: wp
    use lib_statistics

    implicit none

    real(wp) :: empty(0)

    print *, 'MEAN'
    print *, repeat('-', 20)
    print *, mean((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp/))
    print *, mean((/1.0_wp/))
    print *, mean(empty)
    print *, mean(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)))
    print *, mean(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)), 1)
    print *, mean(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)), 2)

    print *, 'STD'
    print *, repeat('-', 20)
    print *, std((/1.0_wp, 2.0_wp, 3.0_wp/))
    print *, std((/1.0_wp, 1.0_wp, 1.0_wp/))
    print *, std((/1.0_wp/))
    print *, std(empty)
    print *, std(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)))
    print *, std(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)), 1)
    print *, std(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)), 2)

end program

