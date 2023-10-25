module lib_linalg

    use lib_kinds, only: wp
    
    implicit none

    private
    public :: eye, diag
    public :: inv_2x2, inv_3x3, inv_4x4

    interface diag
        module procedure diag_r, diag_i
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

    pure function diag_r(d) result(y)
        real(wp), intent(in) :: d(:)
        integer :: i
        real(wp) :: y(size(d), size(d))

        y = 0.0_wp

        do i = 1, size(d)
            y(i, i) = d(i)
        end do

    end function

    pure function inv_2x2(A) result(B)
        ! Performs a direct calculation of the inverse of a 2×2 matrix.
        real(wp), intent(in) :: A(2, 2)   !! Matrix
        real(wp) :: B(2, 2)   !! Inverse matrix
        real(wp) :: detinv

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
        real(wp), intent(in) :: A(3, 3)   !! Matrix
        real(wp) :: B(3, 3)   !! Inverse matrix
        real(wp) :: detinv

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
        real(wp), intent(in) :: A(4, 4)   !! Matrix
        real(wp) :: B(4, 4)   !! Inverse matrix
        real(wp) :: detinv

        ! Calculate the inverse determinant of the matrix
        detinv = &
        1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))) &
          -A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))) &
          +A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))) &
           -A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

        !Calculatetheinverseofthematrix
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

end module

program main_lib_linalg
    use lib_linalg
    use lib_kinds, only: wp
    implicit none

    print *, eye(3)
end program
