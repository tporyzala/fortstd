module lib_math

    implicit none

    interface mean
        module procedure mean_1d, mean_2d, mean_2d_dim
    end interface

    interface std
        module procedure std_1d, std_2d, std_2d_dim
    end interface

    interface diff
        module procedure diff_1d, diff_2d_dim
    end interface

contains

    pure function linspace(a, b, n) result(y)
        real, intent(in) :: a, b
        integer, intent(in) :: n
        real :: y(max(n, 0))
        integer :: i

        if (n < 1) then
            return ! empty y
        end if

        if (n == 1) then
            y(1) = b
            return
        end if

        ! create the linspace
        do i = 1, n
            y(i) = a + (i - 1) * (b - a) / (n - 1)
        end do

    end function

    pure function logspace(a, b, n) result(y)
        real, intent(in) :: a, b
        integer, intent(in) :: n
        real :: y(max(n, 0))

        y = linspace(10.0**a, 10.0**b, n)

    end function

    pure function arrange(a, b, s) result(y)
        real, intent(in) :: a, b, s
        integer :: sz, i
        real :: y(floor((b - a) / s) + 1)

        sz = size(y)
        do i = 1, sz
            y(i) = a + s * real(i - 1)
        end do

    end function

    pure function mean_1d(a) result(y)
        real, intent(in) :: a(:)
        real :: N, y

        N = real(size(a))
        y = sum(a) / max(1.0, N)

    end function

    pure function mean_2d(a) result(y)
        real, intent(in) :: a(:, :)
        real :: N, y

        N = real(size(a))
        y = sum(a) / max(1.0, N)

    end function

    pure function mean_2d_dim(a, dim) result(y)
        real, intent(in) :: a(:, :)
        integer, intent(in) :: dim
        real :: N, y(size(a, 3 - dim))

        N = real(size(a, dim))
        y = sum(a, dim) / max(1.0, N)

    end function

    pure function std_1d(a) result(y)
        real, intent(in) :: a(:)
        real :: N, mu, y

        mu = mean(a)
        N = real(size(a))
        y = sqrt(sum((a - mu)**2) / (max(2.0, N) - 1.0))

    end function

    pure function std_2d(a) result(y)
        real, intent(in) :: a(:, :)
        real :: N, mu, y

        mu = mean(a)
        N = real(size(a))
        y = sqrt(sum((a - mu)**2) / (max(2.0, N) - 1.0))

    end function

    pure function std_2d_dim(a, dim) result(y)
        real, intent(in) :: a(:, :)
        integer, intent(in) :: dim
        integer :: i, sz
        real :: N, mu(size(a, 3 - dim))
        real :: y(size(a, 3 - dim))

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

    pure function diff_1d(a) result(y)
        real, intent(in) :: a(:)
        real :: y(size(a) - 1)
        integer :: i, sz

        sz = size(y)
        do i = 1, sz
            y(i) = a(i + 1) - a(i)
        end do

    end function

    pure function diff_2d_dim(a, dim) result(y)
        real, intent(in) :: a(:, :)
        integer, intent(in) :: dim
        real, allocatable :: y(:, :)
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

    pure function inv_2x2(A) result(B)
        ! Performs a direct calculation of the inverse of a 2×2 matrix.
        complex, intent(in) :: A(2, 2)   !! Matrix
        complex :: B(2, 2)   !! Inverse matrix
        complex :: detinv

        ! Calculate the inverse determinant of the matrix
        detinv = 1 / (A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1))

        ! Calculate the inverse of the matrix
        B(1, 1) = +detinv * A(2, 2)
        B(2, 1) = -detinv * A(2, 1)
        B(1, 2) = -detinv * A(1, 2)
        B(2, 2) = +detinv * A(1, 1)

    end function

    pure function inv_3x3(A) result(B)
        ! Performs a direct calculation of the inverse of a 3×3 matrix.
        complex, intent(in) :: A(3, 3)   !! Matrix
        complex :: B(3, 3)   !! Inverse matrix
        complex :: detinv

        ! Calculate the inverse determinant of the matrix
        detinv = 1 / (A(1, 1) * A(2, 2) * A(3, 3) - A(1, 1) * A(2, 3) * A(3, 2) &
                      - A(1, 2) * A(2, 1) * A(3, 3) + A(1, 2) * A(2, 3) * A(3, 1) &
                      + A(1, 3) * A(2, 1) * A(3, 2) - A(1, 3) * A(2, 2) * A(3, 1))

        ! Calculate the inverse of the matrix
        B(1, 1) = +detinv * (A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2))
        B(2, 1) = -detinv * (A(2, 1) * A(3, 3) - A(2, 3) * A(3, 1))
        B(3, 1) = +detinv * (A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1))
        B(1, 2) = -detinv * (A(1, 2) * A(3, 3) - A(1, 3) * A(3, 2))
        B(2, 2) = +detinv * (A(1, 1) * A(3, 3) - A(1, 3) * A(3, 1))
        B(3, 2) = -detinv * (A(1, 1) * A(3, 2) - A(1, 2) * A(3, 1))
        B(1, 3) = +detinv * (A(1, 2) * A(2, 3) - A(1, 3) * A(2, 2))
        B(2, 3) = -detinv * (A(1, 1) * A(2, 3) - A(1, 3) * A(2, 1))
        B(3, 3) = +detinv * (A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1))

    end function

    pure function inv_4x4(A) result(B)
        ! Performs a direct calculation of the inverse of a 4×4 matrix.
        complex, intent(in) :: A(4, 4)   !! Matrix
        complex :: B(4, 4)   !! Inverse matrix
        complex :: detinv

        ! Calculate the inverse determinant of the matrix
        !&<
        detinv = &
        1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))) &
        -A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))) &
        +A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))) &
        -A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))
        !&>

        !Calculatetheinverseofthematrix
        !&<
      B(1,1)=detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
      B(2,1)=detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
      B(3,1)=detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
      B(4,1)=detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
      B(1,2)=detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
      B(2,2)=detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
      B(3,2)=detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
      B(4,2)=detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
      B(1,3)=detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
      B(2,3)=detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
      B(3,3)=detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
      B(4,3)=detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
      B(1,4)=detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
      B(2,4)=detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
      B(3,4)=detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
      B(4,4)=detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
        !&>

    end function

end module

program main
    use iso_fortran_env
    use lib_math
    implicit none

    real :: empty(0)

    print *, 'LINSPACE'
    print *, repeat('-', 20)
    print *, linspace(0.0, 3.0, 6)
    print *, linspace(0.0, 3.0, 1)
    print *, linspace(0.0, 3.0, 0)

    print *, 'LOGSPACE'
    print *, repeat('-', 20)
    print *, logspace(0.0, 3.0, 6)
    print *, logspace(0.0, 3.0, 1)
    print *, logspace(0.0, 3.0, 0)

    print *, 'ARRANGE'
    print *, repeat('-', 20)
    print *, arrange(1.0, 10.0, 1.0)
    print *, arrange(1.0, 10.0, 2.0)
    print *, arrange(0.0, 10.0, 2.0)

    print *, 'MEAN'
    print *, repeat('-', 20)
    print *, mean((/1.0, 2.0, 3.0, 4.0/))
    print *, mean((/1.0/))
    print *, mean(empty)
    print *, mean(reshape((/1.0, 2.0, 3.0, 4.0, 5.0, 6.0/), (/3, 2/)))
    print *, mean(reshape((/1.0, 2.0, 3.0, 4.0, 5.0, 6.0/), (/3, 2/)), 1)
    print *, mean(reshape((/1.0, 2.0, 3.0, 4.0, 5.0, 6.0/), (/3, 2/)), 2)

    print *, 'STD'
    print *, repeat('-', 20)
    print *, std((/1.0, 2.0, 3.0/))
    print *, std((/1.0, 1.0, 1.0/))
    print *, std((/1.0/))
    print *, std(empty)
    print *, std(reshape((/1.0, 2.0, 3.0, 4.0, 5.0, 6.0/), (/3, 2/)))
    print *, std(reshape((/1.0, 2.0, 3.0, 4.0, 5.0, 6.0/), (/3, 2/)), 1)
    print *, std(reshape((/1.0, 2.0, 3.0, 4.0, 5.0, 6.0/), (/3, 2/)), 2)

    print *, 'DIFF'
    print *, repeat('-', 20)
    print *, diff((/1.0/))
    print *, diff((/1.0, 2.0, 3.0, 5.0, 8.0, 12.0/))
    print *, diff(reshape((/1.0, 2.0, 3.0, 5.0, 8.0, 12.0/), (/3, 2/)), 1)
    print *, diff(reshape((/1.0, 2.0, 3.0, 5.0, 8.0, 12.0/), (/3, 2/)), 2)

end program
