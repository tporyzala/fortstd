program test_do
    ! https://stackoverflow.com/questions/29928293/does-gfortran-take-advantage-of-do-concurrent
    ! https://curc.readthedocs.io/en/latest/programming/OpenMP-Fortran.html
    ! gfortran do_concurrent.f08 -fopenmp -Ofast -ftree-parallelize-loops=6
    ! https://hpc-tutorials.llnl.gov/openmp/

    use omp_lib, only: omp_get_wtime

    integer, parameter :: n = 1000000, m = 1000
    real, allocatable :: q(:)

    integer :: i
    real*8 :: t0

    allocate (q(n))

    t0 = omp_get_wtime()
    do i = 1, n
        q(i) = i
        do j = 1, m
            q(i) = 0.5 * (q(i) + i / q(i))
        end do
    end do
    print *, omp_get_wtime() - t0

    t0 = omp_get_wtime()
    do concurrent(i=1:n)
        q(i) = i
        do j = 1, m
            q(i) = 0.5 * (q(i) + i / q(i))
        end do
    end do
    print *, omp_get_wtime() - t0

end program test_do
