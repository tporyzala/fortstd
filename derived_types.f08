program struct
    use iso_fortran_env
    implicit none

    type my_struct
        real*16 :: x
        integer*16 :: i

    end type my_struct

    type my_struct2
        real*8 :: x
        integer*8 :: i

    end type my_struct2

    type(my_struct) :: s1
    type(my_struct2) :: s2

    s1 % x = huge(s1 % x)
    s1 % i = huge(s1 % i)

    s2 % x = huge(s2 % x)
    s2 % i = huge(s2 % i)

    print *, 's1.x', s1 % x
    print *, 's1.i', s1 % i

    print *, 's2.x', s2 % x
    print *, 's2.i', s2 % i

    ! kinds are promoted
    print *, 'kind(s1.x)', kind(s1 % x)
    print *, 'kind(s2.x)', kind(s2 % x)
    print *, 'kind( s1.x / s2.x )', kind(s1 % x / s2 % x)

end program struct
