program test
    use lib_kinds
    use lib_array, only: linspace
    use lib_linalg
    use omp_lib
    implicit none

    real(dp) :: t1, t2
    integer(int64) :: i

    real(dp) :: A(3, 3), L(3, 3), y(3), x(3), b(3)
    A = reshape(real((/4, 12, -16, 12, 37, -43, -16, -43, 98/), kind=kind(A)), (/3, 3/))
    b = real((/1, 2, 3/), kind=kind(A))

    ! L = chol(A)
    ! print *, L(1, :)
    ! print *, L(2, :)
    ! print *, L(3, :)

    ! print *, norm2(matmul(L, transpose(L)) - A)

    ! y = forward_solve(L, b)
    ! x = backward_solve(transpose(L), y)

    ! print *, y
    ! print *, x

    x = 0.0_sp
    call cpu_time(t1)
    do i = 1, 10000000
        x = matmul(inv_3x3(A), b)
    end do
    call cpu_time(t2)
    print *, t2 - t1

    x = 0.0_sp
    call cpu_time(t1)
    do i = 1, 10000000
        L = chol(A)
        y = forward_solve(L, b)
        x = backward_solve(transpose(L), y)
    end do
    call cpu_time(t2)
    print *, t2 - t1

end program
