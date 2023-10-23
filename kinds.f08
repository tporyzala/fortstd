program kinds

    use iso_fortran_env

    implicit none

    real :: x_real = sqrt(2.0)
    real*4 :: x_real4 = sqrt(2.0)
    real*8 :: x_real8 = sqrt(2.0)
    real*16 :: x_real16 = sqrt(2.0)

    real(kind=selected_real_kind(1)) :: srk1 = sqrt(2.0)
    real(kind=selected_real_kind(10)) :: srk10 = sqrt(2.0)
    real(kind=selected_real_kind(32)) :: srk32 = sqrt(2.0)

    real(real_kinds(1)) :: x_rk1 = sqrt(2.0)
    real(real_kinds(2)) :: x_rk2 = sqrt(2.0)
    real(real_kinds(3)) :: x_rk3 = sqrt(2.0)
    real(real_kinds(4)) :: x_rk4 = sqrt(2.0)

    integer :: x_int
    integer*1 :: x_int1
    integer*2 :: x_int2
    integer*4 :: x_int4
    integer*8 :: x_int8
    integer*16 :: x_int16

    logical*1 :: x_log1
    logical*2 :: x_log2

    ! Print kinds from iso_fortran_env
    print *, 'Logical  : ', logical_kinds
    print *, 'Character: ', character_kinds
    print *, 'Integer  : ', integer_kinds
    print *, 'Real     : ', real_kinds

    ! Print explciteily defined kinds
    print *, 'Real     :  ', precision(x_real), x_real
    print *, 'Real*4   :  ', precision(x_real4), x_real4
    print *, 'Real*8   :  ', precision(x_real8), x_real8
    print *, 'Real*16  :  ', precision(x_real16), x_real16

    ! print selected real kinds
    print *, 'srk1     :  ', precision(srk1), srk1
    print *, 'srk10    :  ', precision(srk10), srk10
    print *, 'srk32    :  ', precision(srk32), srk32

    ! print real kinds
    print *, 'rk1      :  ', precision(x_rk1), x_rk1
    print *, 'rk2      :  ', precision(x_rk2), x_rk2
    print *, 'rk3      :  ', precision(x_rk3), x_rk3
    print *, 'rk4      :  ', precision(x_rk4), x_rk4

    ! print integers
    print *, 'int      :  ', huge(x_int)
    print *, 'int1     :  ', huge(x_int1)
    print *, 'int2     :  ', huge(x_int2)
    print *, 'int4     :  ', huge(x_int4)
    print *, 'int8     :  ', huge(x_int8)
    print *, 'int16    :  ', huge(x_int16)

    print *, 'log1     :  ', x_log1
    print *, 'log2     :  ', x_log2

end program

