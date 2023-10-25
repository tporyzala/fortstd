module lib_kinds

    use iso_fortran_env, only: &
        int8, int16, int32, int64, real32, real64, real128

    use iso_c_binding, only: &
        c_bool, c_char, c_double, c_float

    implicit none

    ! Possible working kinds
    integer, parameter :: sp = selected_real_kind(6)
    integer, parameter :: dp = selected_real_kind(15)
    integer, parameter :: xdp = selected_real_kind(18)
    integer, parameter :: qp = selected_real_kind(33)
    integer, parameter :: lk = kind(.true.)

    ! Set WORKING real kind
    integer, parameter :: wp = dp

end module
