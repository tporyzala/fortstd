program kinds

    use iso_fortran_env

    implicit none

    real :: x_real = sqrt(2.0)
    real*4 :: x_real4 = sqrt(2.0)
    real*8 :: x_real8 = sqrt(2.0)
    real*16 :: x_real16 = sqrt(2.0)

    integer :: x_int
    integer*1 :: x_int1
    integer*2 :: x_int2
    integer*4 :: x_int4
    integer*8 :: x_int8
    integer*16 :: x_int16

    logical*1 :: x_log1
    logical*2 :: x_log2
    
    real(kind=selected_real_kind(1)) :: srk1 = sqrt(2.0)
    real(kind=selected_real_kind(10)) :: srk2 = sqrt(2.0)
    real(kind=selected_real_kind(32)) :: srk3 = sqrt(2.0)

    ! Print kinds from iso_fortran_env
    print *, 'Logical  : ', logical_kinds
    print *, 'Character: ', character_kinds
    print *, 'Integer  : ', integer_kinds
    print *, 'Real     : ', real_kinds

    ! Print explciteily defined kinds
    print *, 'Real     :  ', x_real
    print *, 'Real*4   :  ', x_real4
    print *, 'Real*8   :  ', x_real8
    print *, 'Real*16  :  ', x_real16

    ! print selected real kinds
    print *, 'srk1     :  ', srk1
    print *, 'srk2     :  ', srk2
    print *, 'srk3     :  ', srk3

    ! print integers
    print *, 'int      :  ', huge(x_int)
    print *, 'int1     :  ', huge(x_int1)
    print *, 'int2     :  ', huge(x_int2)
    print *, 'int4     :  ', huge(x_int4)
    print *, 'int8     :  ', huge(x_int8)
    print *, 'int16    :  ', huge(x_int16)

    print *, 'log1    :  ', x_log1
    print *, 'log2    :  ', x_log2

end program

