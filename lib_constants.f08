module lib_constants

    use lib_kinds, only: sp, dp

    implicit none

    real(sp), parameter :: pi_sp = 3.141592653589793238462643383279_sp
    real(dp), parameter :: pi_dp = 3.141592653589793238462643383279_dp

    real(sp), parameter :: e_sp = 2.718281828459045235360287471352_sp
    real(dp), parameter :: e_dp = 2.718281828459045235360287471352_dp

    real(sp), parameter :: c_sp = 299792458.0_sp ! m/s
    real(dp), parameter :: c_dp = 299792458.0_dp ! m/s

    complex(sp), parameter :: i_sp = (0.0_sp, 1.0_sp)
    complex(dp), parameter :: i_dp = (0.0_dp, 1.0_dp)

    real(sp), parameter :: eps_sp = tiny(1.0_sp)
    real(dp), parameter :: eps_dp = tiny(1.0_dp)

end module

