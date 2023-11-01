module lib_random

    implicit none

    private
    public :: rand_norm

contains

    function rand_norm() result(fn_val)
        ! This subroutine returns a single psuedo-random value
        ! from a normal distribution mean=0, std=1
        !
        ! Adapted from: http://jblevins.org/mirror/amiller/random.f90
        real :: fn_val
        real :: s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472, &
                r1 = 0.27597, r2 = 0.27846, u, v, x, y, q

        ! Generate P = (u,v) uniform in rectangle enclosing acceptance region

        do
            call random_number(u)
            call random_number(v)
            v = 1.7156 * (v - 0.5)

            ! Evaluate the quadratic form
            x = u - s
            y = abs(v) - t
            q = x**2 + y * (a * y - b * x)

            ! Accept P if inside inner ellipse
            if (q < r1) exit
            ! Reject P if outside outer ellipse
            if (q > r2) cycle
            ! Reject P if outside acceptance region
            if (v**2 < -4.0 * log(u) * u**2) exit
        end do

        ! Return ratio of P's coordinates as the normal deviate
        fn_val = v / u

    end function

end module
