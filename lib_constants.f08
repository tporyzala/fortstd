module lib_constants

    use lib_kinds

    implicit none

    real(wp), parameter :: pi = 3.141592653589793238462643383279_wp
    real(wp), parameter :: e_ = 2.718281828459045235360287471352_wp

    real(wp), parameter :: c_ = 299792458.0_wp ! m/s
    complex(wp), parameter :: i_ = (0, 1)

    real(wp), parameter :: eps_ = tiny(1.0_wp)

contains

    subroutine print_constants
        implicit none
        print *, 'pi    ', pi
        print *, 'e_    ', e_
        print *, 'c_    ', c_
        print *, 'i_    ', i_
        print *, 'eps_  ', eps_
    end subroutine print_constants

end module

