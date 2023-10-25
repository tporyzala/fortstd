module lib_array

    use lib_kinds, only: sp, dp
    
    implicit none

    private
    public :: linspace, logspace, arrange, diff, unique

    interface linspace
        module procedure linspace_sp
        module procedure linspace_dp
    end interface

    interface logspace
        module procedure logspace_sp
        module procedure logspace_dp
        module procedure logspace_base_sp
        module procedure logspace_base_dp
    end interface

    interface arrange
        module procedure arrange_r_sp
        module procedure arrange_r_dp
        module procedure arrange_i
    end interface

    interface diff
        module procedure diff_1d_sp
        module procedure diff_1d_dp
        module procedure diff_2d_dim_sp
        module procedure diff_2d_dim_dp
    end interface

    interface unique
        module procedure unique_1d_sp
        module procedure unique_1d_dp
        module procedure unique_2d_sp
        module procedure unique_2d_dp
    end interface

contains

    pure function linspace_sp(a, b, n) result(y)
        real(sp), intent(in) :: a, b
        integer, intent(in) :: n
        real(sp) :: y(max(n, 0))
        integer :: i

        if (n < 1) then
            return ! empty y
        end if

        if (n == 1) then
            y(1) = b
            return
        end if

        do i = 1, n
            y(i) = a + real((i - 1), kind=sp) * (b - a) / real((n - 1), kind=sp)
        end do

    end function

    pure function linspace_dp(a, b, n) result(y)
        real(dp), intent(in) :: a, b
        integer, intent(in) :: n
        real(dp) :: y(max(n, 0))
        integer :: i

        if (n < 1) then
            return ! empty y
        end if

        if (n == 1) then
            y(1) = b
            return
        end if

        do i = 1, n
            y(i) = a + real((i - 1), kind=dp) * (b - a) / real((n - 1), kind=dp)
        end do

    end function

    pure function logspace_sp(a, b, n) result(y)
        real(sp), intent(in) :: a, b
        integer, intent(in) :: n
        real(sp) :: y(max(n, 0))

        y = linspace(a, b, n)
        y = 10.0_sp**y

    end function

    pure function logspace_dp(a, b, n) result(y)
        real(dp), intent(in) :: a, b
        integer, intent(in) :: n
        real(dp) :: y(max(n, 0))

        y = linspace(a, b, n)
        y = 10.0_dp**y

    end function

    pure function logspace_base_sp(a, b, n, base) result(y)
        real(sp), intent(in) :: a, b
        integer, intent(in) :: n
        real(sp), intent(in) :: base
        real(sp) :: y(max(n, 0))

        y = linspace(base**a, base**b, n)

    end function

    pure function logspace_base_dp(a, b, n, base) result(y)
        real(dp), intent(in) :: a, b
        integer, intent(in) :: n
        real(dp), intent(in) :: base
        real(dp) :: y(max(n, 0))

        y = linspace(base**a, base**b, n)

    end function

    pure function arrange_r_sp(a, b, s) result(y)
        real(sp), intent(in) :: a, b, s
        integer :: sz, i
        real(sp) :: y(floor((b - a) / s) + 1)

        sz = size(y)

        do i = 1, sz
            y(i) = a + s * real(i - 1, kind=sp)
        end do

    end function

    pure function arrange_r_dp(a, b, s) result(y)
        real(dp), intent(in) :: a, b, s
        integer :: sz, i
        real(dp) :: y(floor((b - a) / s) + 1)

        sz = size(y)

        do i = 1, sz
            y(i) = a + s * real(i - 1, kind=dp)
        end do

    end function

    pure function arrange_i(a, b, s) result(y)
        integer, intent(in) :: a, b, s
        integer :: sz, i
        integer :: y(floor((b - a) / s * 1.0_sp) + 1)

        sz = size(y)

        do i = 1, sz
            y(i) = a + s * (i - 1)
        end do

    end function

    pure function diff_1d_sp(a) result(y)
        real(sp), intent(in) :: a(:)
        real(sp) :: y(size(a) - 1)
        integer :: i, sz

        sz = size(y)
        do i = 1, sz
            y(i) = a(i + 1) - a(i)
        end do

    end function

    pure function diff_1d_dp(a) result(y)
        real(dp), intent(in) :: a(:)
        real(dp) :: y(size(a) - 1)
        integer :: i, sz

        sz = size(y)
        do i = 1, sz
            y(i) = a(i + 1) - a(i)
        end do

    end function

    pure function diff_2d_dim_sp(a, dim) result(y)
        real(sp), intent(in) :: a(:, :)
        integer, intent(in) :: dim
        real(sp), allocatable :: y(:, :)
        integer :: i, sz

        if (dim == 1) then
            allocate (y(size(a, 1) - 1, size(a, 2)))
            sz = size(y, 1)
            do i = 1, sz
                y(i, :) = a(i + 1, :) - a(i, :)
            end do
        else if (dim == 2) then
            allocate (y(size(a, 1), size(a, 2) - 1))
            sz = size(y, 2)
            do i = 1, sz
                y(:, i) = a(:, i + 1) - a(:, i)
            end do
        end if

    end function

    pure function diff_2d_dim_dp(a, dim) result(y)
        real(dp), intent(in) :: a(:, :)
        integer, intent(in) :: dim
        real(dp), allocatable :: y(:, :)
        integer :: i, sz

        if (dim == 1) then
            allocate (y(size(a, 1) - 1, size(a, 2)))
            sz = size(y, 1)
            do i = 1, sz
                y(i, :) = a(i + 1, :) - a(i, :)
            end do
        else if (dim == 2) then
            allocate (y(size(a, 1), size(a, 2) - 1))
            sz = size(y, 2)
            do i = 1, sz
                y(:, i) = a(:, i + 1) - a(:, i)
            end do
        end if

    end function

    pure function unique_1d_sp(a) result(y)
        real(sp), intent(in) :: a(:)
        logical :: mask(size(a)), work(size(a))
        integer :: i
        real(sp), allocatable :: y(:)

        mask = .false.

        do i = 1, size(a)
            work = a == a(i)
            if (count(work) == 1) then
                ! one value, flag it
                mask(i) = .true.
            else
                ! multiple values, flag it only if it hasnt been seen before
                if (.not. any(work .and. mask)) then
                    mask(i) = .true.
                end if
            end if
        end do

        allocate (y(count(mask)))
        y = pack(a, mask=mask)

    end function

    pure function unique_1d_dp(a) result(y)
        real(dp), intent(in) :: a(:)
        logical :: mask(size(a)), work(size(a))
        integer :: i
        real(dp), allocatable :: y(:)

        mask = .false.

        do i = 1, size(a)
            work = a == a(i)
            if (count(work) == 1) then
                ! one value, flag it
                mask(i) = .true.
            else
                ! multiple values, flag it only if it hasnt been seen before
                if (.not. any(work .and. mask)) then
                    mask(i) = .true.
                end if
            end if
        end do

        allocate (y(count(mask)))
        y = pack(a, mask=mask)

    end function

    pure function unique_2d_sp(a2) result(y)
        real(sp), intent(in) :: a2(:, :)
        real(sp) :: a(size(a2))
        logical :: mask(size(a)), work(size(a))
        integer :: i
        real(sp), allocatable :: y(:)

        a = pack(a2, .true.)
        mask = .false.

        do i = 1, size(a)
            work = a == a(i)
            if (count(work) == 1) then
                ! one value, flag it
                mask(i) = .true.
            else
                ! multiple values, flag it only if it hasnt been flagged before
                if (.not. any(work .and. mask)) then
                    mask(i) = .true.
                end if
            end if
        end do

        allocate (y(count(mask)))
        y = pack(a, mask=mask)

    end function

    pure function unique_2d_dp(a2) result(y)
        real(dp), intent(in) :: a2(:, :)
        real(dp) :: a(size(a2))
        logical :: mask(size(a)), work(size(a))
        integer :: i
        real(dp), allocatable :: y(:)

        a = pack(a2, .true.)
        mask = .false.

        do i = 1, size(a)
            work = a == a(i)
            if (count(work) == 1) then
                ! one value, flag it
                mask(i) = .true.
            else
                ! multiple values, flag it only if it hasnt been flagged before
                if (.not. any(work .and. mask)) then
                    mask(i) = .true.
                end if
            end if
        end do

        allocate (y(count(mask)))
        y = pack(a, mask=mask)

    end function

end module

! program main_lib_array

!     use lib_array
!     use lib_kinds, only: wp => sp
!     implicit none

!     print *, 'LINSPACE'
!     print *, repeat('-', 20)
!     print *, linspace(0.0_wp, 3.0_wp, 6)
!     print *, linspace(0.0_wp, 3.0_wp, 1)
!     print *, linspace(0.0_wp, 3.0_wp, 0)

!     print *, 'LOGSPACE'
!     print *, repeat('-', 20)
!     print *, logspace(0.0_wp, 1.0_wp, 3)
!     print *, logspace(0.0_wp, 1.0_wp, 1)
!     print *, logspace(0.0_wp, 1.0_wp, 0)

!     print *, 'ARRANGE'
!     print *, repeat('-', 20)
!     print *, arrange(1.0_wp, 5.0_wp, 1.0_wp)
!     print *, arrange(1.0_wp, 5.0_wp, 2.0_wp)
!     print *, arrange(0.0_wp, 5.0_wp, 2.0_wp)
!     print *, arrange(0, 10, 1)
!     print *, arrange(0, 10, 2)
!     print *, arrange(1, 10, 2)

!     print *, 'DIFF'
!     print *, repeat('-', 20)
!     print *, diff((/1.0_wp/))
!     print *, diff((/1.0_wp, 2.0_wp, 3.0_wp, 5.0_wp, 8.0_wp, 12.0_wp/))
!     print *, diff(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 5.0_wp, 8.0_wp, 12.0_wp/), (/3, 2/)), 1)
!     print *, diff(reshape((/1.0_wp, 2.0_wp, 3.0_wp, 5.0_wp, 8.0_wp, 12.0_wp/), (/3, 2/)), 2)

!     print *, 'UNIQUE'
!     print *, repeat('-', 20)
!     print *, unique((/2.0_wp, 2.0_wp, 1.0_wp, 2.0_wp, -1.0_wp, 22.0_wp, 1.0_wp, 3.0_wp/))
!     print *, unique(reshape((/2.0_wp, 2.0_wp, 1.0_wp, 2.0_wp, -1.0_wp, 22.0_wp, 1.0_wp, 3.0_wp/), (/4, 2/)))

! end program
