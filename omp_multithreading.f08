program Parallel_Stored_Hello
    ! https://curc.readthedocs.io/en/latest/programming/OpenMP-Fortran.html
    ! https://hpc-tutorials.llnl.gov/openmp/
    use omp_lib

    integer :: thread_id
    real*8 :: t0, t

    t0 = omp_get_wtime()
    !$omp parallel private(thread_id)

    thread_id = omp_get_thread_num()
    print *, 'Hello from process:', thread_id

    !$omp end parallel
    t = omp_get_wtime() - t0
    print *, 'Wall time: ', t, ' [sec]'

end
