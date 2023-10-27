module lib_linalg

    use lib_kinds, only: sp, dp

    implicit none

    private
    public :: eye, diag
    public :: cross
    public :: inv_2x2, inv_3x3, inv_4x4
    public :: chol, forward_solve, backward_solve

    interface diag
        module procedure diag_i
        module procedure diag_sp
        module procedure diag_dp
    end interface

    interface cross
        module procedure cross_sp
        module procedure cross_dp
    end interface

    interface inv_2x2
        module procedure inv_2x2_sp
        module procedure inv_2x2_dp
    end interface

    interface inv_3x3
        module procedure inv_3x3_sp
        module procedure inv_3x3_dp
    end interface

    interface inv_4x4
        module procedure inv_4x4_sp
        module procedure inv_4x4_dp
    end interface

    interface chol
        module procedure chol_sp
        module procedure chol_dp
    end interface

    interface forward_solve
        module procedure forward_solve_sp
        module procedure forward_solve_dp
    end interface

    interface backward_solve
        module procedure backward_solve_sp
        module procedure backward_solve_dp
    end interface

contains

    pure function eye(n) result(y)
        integer, intent(in) :: n
        integer :: i
        integer :: y(n, n)

        y = 0

        do i = 1, n
            y(i, i) = 1
        end do

    end function

    pure function diag_i(d) result(y)
        integer, intent(in) :: d(:)
        integer :: i
        integer :: y(size(d), size(d))

        y = 0

        do i = 1, size(d)
            y(i, i) = d(i)
        end do

    end function

    pure function diag_sp(d) result(y)
        real(sp), intent(in) :: d(:)
        integer :: i
        real(sp) :: y(size(d), size(d))

        y = 0.0_sp

        do i = 1, size(d)
            y(i, i) = d(i)
        end do

    end function

    pure function diag_dp(d) result(y)
        real(dp), intent(in) :: d(:)
        integer :: i
        real(dp) :: y(size(d), size(d))

        y = 0.0_dp

        do i = 1, size(d)
            y(i, i) = d(i)
        end do

    end function

    pure function cross_sp(a, b) result(c)
        real(sp), intent(in) :: a(3), b(3)
        real(sp) :: c(3)

        c(1) = a(2) * b(3) - a(3) * b(2)
        c(2) = a(3) * b(1) - a(1) * b(3)
        c(3) = a(1) * b(2) - a(2) * b(1)

    end function

    pure function cross_dp(a, b) result(c)
        real(dp), intent(in) :: a(3), b(3)
        real(dp) :: c(3)

        c(1) = a(2) * b(3) - a(3) * b(2)
        c(2) = a(3) * b(1) - a(1) * b(3)
        c(3) = a(1) * b(2) - a(2) * b(1)

    end function

    pure function inv_2x2_sp(A) result(B)
        ! Performs a direct calculation of the inverse of a 2×2 matrix.
        real(sp), intent(in) :: A(2, 2)   !! Matrix
        real(sp) :: B(2, 2)   !! Inverse matrix
        real(sp) :: detinv

        ! Calculate the inverse determinant of the matrix
        detinv = 1 / (A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1))

        ! Calculate the inverse of the matrix
        B(1, 1) = +detinv * A(2, 2)
        B(2, 1) = -detinv * A(2, 1)
        B(1, 2) = -detinv * A(1, 2)
        B(2, 2) = +detinv * A(1, 1)

    end function

    pure function inv_2x2_dp(A) result(B)
        ! Performs a direct calculation of the inverse of a 2×2 matrix.
        real(dp), intent(in) :: A(2, 2)   !! Matrix
        real(dp) :: B(2, 2)   !! Inverse matrix
        real(dp) :: detinv

        ! Calculate the inverse determinant of the matrix
        detinv = 1 / (A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1))

        ! Calculate the inverse of the matrix
        B(1, 1) = +detinv * A(2, 2)
        B(2, 1) = -detinv * A(2, 1)
        B(1, 2) = -detinv * A(1, 2)
        B(2, 2) = +detinv * A(1, 1)

    end function

    pure function inv_3x3_sp(A) result(B)
        ! Performs a direct calculation of the inverse of a 3×3 matrix.
        real(sp), intent(in) :: A(3, 3)   !! Matrix
        real(sp) :: B(3, 3)   !! Inverse matrix
        real(sp) :: detinv

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

    pure function inv_3x3_dp(A) result(B)
        ! Performs a direct calculation of the inverse of a 3×3 matrix.
        real(dp), intent(in) :: A(3, 3)   !! Matrix
        real(dp) :: B(3, 3)   !! Inverse matrix
        real(dp) :: detinv

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

    pure function inv_4x4_sp(A) result(B)
        ! Performs a direct calculation of the inverse of a 4×4 matrix.
        real(sp), intent(in) :: A(4, 4)   !! Matrix
        real(sp) :: B(4, 4)   !! Inverse matrix
        real(sp) :: detinv

        ! Calculate the inverse determinant of the matrix
        detinv = &
        1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))) &
          -A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))) &
          +A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))) &
           -A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

        !Calculate the inverse of thematrix
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

    end function

    pure function inv_4x4_dp(A) result(B)
        ! Performs a direct calculation of the inverse of a 4×4 matrix.
        real(dp), intent(in) :: A(4, 4)   !! Matrix
        real(dp) :: B(4, 4)   !! Inverse matrix
        real(dp) :: detinv

        ! Calculate the inverse determinant of the matrix
        detinv = &
        1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))) &
          -A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))) &
          +A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))) &
           -A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

        !Calculate the inverse of thematrix
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

    end function

    pure function chol_sp(A) result(L)
        real(sp), intent(in) :: A(:, :)
        real(sp) :: s
        integer :: i, j
        real(sp) :: L(size(A, 1), size(A, 1))

        L = 0.0_sp

        do i = 1, size(A, 1)
            do j = 1, i
                s = sum(L(i, 1:j-1) * L(j, 1:j-1))
                if (i == j) then
                    L(i, j) = sqrt(A(i, i) - s)
                else
                    L(i, j) = 1.0_sp / L(j, j) * (A(i, j) - s)
                end if
            end do
        end do
    end function

    pure function chol_dp(A) result(L)
        real(dp), intent(in) :: A(:, :)
        real(dp) :: s
        integer :: i, j
        real(dp) :: L(size(A, 1), size(A, 1))

        L = 0.0_dp

        do i = 1, size(A, 1)
            do j = 1, i
                s = sum(L(i, 1:j-1) * L(j, 1:j-1))
                if (i == j) then
                    L(i, j) = sqrt(A(i, i) - s)
                else
                    L(i, j) = 1.0_dp / L(j, j) * (A(i, j) - s)
                end if
            end do
        end do
    end function

    pure function forward_solve_sp(L, b) result(x)
        real(sp), intent(in) :: L(:, :), b(:)
        integer :: i
        real(sp) :: s
        real(sp) :: x(size(b))

        do i = 1, size(L, 1)
            s = sum(L(i, 1:i - 1) * x(1:i - 1))
            x(i) = (b(i) - s) / L(i, i)
        end do

    end function

    pure function forward_solve_dp(L, b) result(x)
        real(dp), intent(in) :: L(:, :), b(:)
        integer :: i
        real(dp) :: s
        real(dp) :: x(size(b))

        do i = 1, size(L, 1)
            s = sum(L(i, 1:i - 1) * x(1:i - 1))
            x(i) = (b(i) - s) / L(i, i)
        end do

    end function

    pure function backward_solve_sp(U, b) result(x)
        real(sp), intent(in) :: U(:, :), b(:)
        integer :: i
        real(sp) :: s
        real(sp) :: x(size(b))

        do i = size(U, 1), 1, -1
            s = sum(U(i, (i + 1):) * x((i + 1):))
            x(i) = (b(i) - s) / U(i, i)
        end do

    end function

    pure function backward_solve_dp(U, b) result(x)
        real(dp), intent(in) :: U(:, :), b(:)
        integer :: i
        real(dp) :: s
        real(dp) :: x(size(b))

        do i = size(U, 1), 1, -1
            s = sum(U(i, (i + 1):) * x((i + 1):))
            x(i) = (b(i) - s) / U(i, i)
        end do

    end function

end module
