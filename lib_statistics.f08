module lib_statistics

    use lib_kinds, only: sp, dp

    implicit none

    private
    public :: mean, std

    interface mean
        module procedure mean_1d_sp
        module procedure mean_1d_dp
        module procedure mean_2d_sp
        module procedure mean_2d_dp
        module procedure mean_2d_dim_sp
        module procedure mean_2d_dim_dp
    end interface

    interface std
        module procedure std_1d_sp
        module procedure std_1d_dp
        module procedure std_2d_sp
        module procedure std_2d_dp
        module procedure std_2d_dim_sp
        module procedure std_2d_dim_dp
    end interface

contains

    pure function mean_1d_sp(a) result(y)
        real(sp), intent(in) :: a(:)
        real(sp) :: N, y

        N = real(size(a))
        y = sum(a) / max(1.0_sp, N)

    end function

    pure function mean_1d_dp(a) result(y)
        real(dp), intent(in) :: a(:)
        real(dp) :: N, y

        N = real(size(a))
        y = sum(a) / max(1.0_dp, N)

    end function

    pure function mean_2d_sp(a) result(y)
        real(sp), intent(in) :: a(:, :)
        real(sp) :: N, y

        N = real(size(a))
        y = sum(a) / max(1.0_sp, N)

    end function

    pure function mean_2d_dp(a) result(y)
        real(dp), intent(in) :: a(:, :)
        real(dp) :: N, y

        N = real(size(a))
        y = sum(a) / max(1.0_dp, N)

    end function

    pure function mean_2d_dim_sp(a, dim) result(y)
        real(sp), intent(in) :: a(:, :)
        integer, intent(in) :: dim
        real(sp) :: N, y(size(a, 3 - dim))

        N = real(size(a, dim), kind=sp)
        y = sum(a, dim) / max(1.0_sp, N)

    end function

    pure function mean_2d_dim_dp(a, dim) result(y)
        real(dp), intent(in) :: a(:, :)
        integer, intent(in) :: dim
        real(dp) :: N, y(size(a, 3 - dim))

        N = real(size(a, dim), kind=dp)
        y = sum(a, dim) / max(1.0_dp, N)

    end function

    pure function std_1d_sp(a) result(y)
        real(sp), intent(in) :: a(:)
        real(sp) :: N, mu, y

        mu = mean(a)
        N = real(size(a), kind=sp)
        y = sqrt(sum((a - mu)**2) / (max(2.0_sp, N) - 1.0_sp))

    end function

    pure function std_1d_dp(a) result(y)
        real(dp), intent(in) :: a(:)
        real(dp) :: N, mu, y

        mu = mean(a)
        N = real(size(a), kind=dp)
        y = sqrt(sum((a - mu)**2) / (max(2.0_dp, N) - 1.0_dp))

    end function

    pure function std_2d_sp(a) result(y)
        real(sp), intent(in) :: a(:, :)
        real(sp) :: N, mu, y

        mu = mean(a)
        N = real(size(a), kind=sp)
        y = sqrt(sum((a - mu)**2) / (max(2.0_sp, N) - 1.0_sp))

    end function

    pure function std_2d_dp(a) result(y)
        real(dp), intent(in) :: a(:, :)
        real(dp) :: N, mu, y

        mu = mean(a)
        N = real(size(a), kind=dp)
        y = sqrt(sum((a - mu)**2) / (max(2.0_dp, N) - 1.0_dp))

    end function

    pure function std_2d_dim_sp(a, dim) result(y)
        real(sp), intent(in) :: a(:, :)
        integer, intent(in) :: dim
        integer :: i, sz
        real(sp) :: N, mu(size(a, 3 - dim))
        real(sp) :: y(size(a, 3 - dim))

        mu = mean(a, dim)
        N = real(size(a, dim), kind=sp)
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
        y = sqrt(y / (max(2.0_sp, N) - 1.0_sp))

    end function

    pure function std_2d_dim_dp(a, dim) result(y)
        real(dp), intent(in) :: a(:, :)
        integer, intent(in) :: dim
        integer :: i, sz
        real(dp) :: N, mu(size(a, 3 - dim))
        real(dp) :: y(size(a, 3 - dim))

        mu = mean(a, dim)
        N = real(size(a, dim), kind=dp)
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
        y = sqrt(y / (max(2.0_dp, N) - 1.0_dp))

    end function

end module

! program main

!     use lib_kinds, only: wp=>sp
!     use lib_statistics

!     implicit none

!     real(wp) :: empty(0)

!     print *, 'MEAN'
!     print *, repeat('-', 20)
!     print *, mean((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp/))
!     print *, mean((/1.0_wp/))
!     print *, mean(empty)
!     print *, mean(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)))
!     print *, mean(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)), 1)
!     print *, mean(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)), 2)

!     print *, 'STD'
!     print *, repeat('-', 20)
!     print *, std((/1.0_wp, 2.0_wp, 3.0_wp/))
!     print *, std((/1.0_wp, 1.0_wp, 1.0_wp/))
!     print *, std((/1.0_wp/))
!     print *, std(empty)
!     print *, std(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)))
!     print *, std(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)), 1)
!     print *, std(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp/), (/3, 2/)), 2)

! end program

