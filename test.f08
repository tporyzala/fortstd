program test
    use lib_kinds, only: dp
    use lib_array, only: linspace
    use lib_linalg, only: dot
    implicit none

    integer, parameter :: n = 50
    integer,parameter :: iter = 10000000
    real(dp) :: a(n), b(n), c, t1, t2
    integer :: i, j

    a = linspace(0.0_dp, 100.0_dp, n)
    b = linspace(0.0_dp, 100.0_dp, n)

    call cpu_time(t1)
    do i = 1,iter
        c = dot_product(a, b)
    end do
    call cpu_time(t2)
    print *, t2 - t1

    call cpu_time(t1)
    do i = 1,iter
        c = dot(a, b)
    end do
    call cpu_time(t2)
    print *, t2 - t1

end program
