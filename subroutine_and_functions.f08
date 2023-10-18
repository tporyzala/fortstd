
pure real function pure_add(a, b) result(c)
    real, intent(in) :: a, b
    c = a + b
end function

real function side_effects_add(a, b) result(c)
    real :: a, b
    c = a + b
    a = 10
    b = 10
end function

subroutine squared(a)
    real, intent(inout) :: a
    a = a**2
end subroutine

program sub_and_func
    use iso_fortran_env
    implicit none

    real :: pure_add, side_effects_add, a, b
    a = 1
    b = 2

    print *, 'Pure function, before and after'
    print *, a, b
    print *, pure_add(a, b)
    print *, a, b

    print *, 'Side effects function, before and after'
    print *, a, b
    print *, side_effects_add(a, b)
    print *, a, b

    print *, 'subroutine'
    call squared(a)
    print *, a

end program

