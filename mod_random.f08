module mod_random
	!--------------------------------------------------
	! This module is useful for generating different types of
	! random variables beyond random_number().
	!--------------------------------------------------
	implicit none
	contains
	
	subroutine random_normal(fn_val)
		!--------------------------------------------------
		! adapted from: http://jblevins.org/mirror/amiller/random.f90
		!
		! This subroutine returns a single psuedo-random value
		! from a normal distribution mean=0, std=1
		!--------------------------------------------------
		implicit none
		real,intent(inout) :: fn_val
		
		! Local variables
		real	:: 	s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472,    &
					r1 = 0.27597, r2 = 0.27846, u, v, x, y, q
		
		! Generate P = (u,v) uniform in rectangle enclosing acceptance region
		
		do
			call random_number(u)
			call random_number(v)
			v = 1.7156 * (v - 0.5)
			
			! Evaluate the quadratic form
			x = u - s
			y = abs(v) - t
			q = x**2 + y*(a*y - b*x)
			
			! Accept P if inside inner ellipse
			if (q < r1) exit
			!     Reject P if outside outer ellipse
			if (q > r2) cycle
			!     Reject P if outside acceptance region
			if (v**2 < -4.0*LOG(u)*u**2) EXIT
		end do
		
		! Return ratio of P's coordinates as the normal deviate
		fn_val = v/u
		
	end subroutine random_normal
	
	subroutine init_random_seed()
		!--------------------------------------------------
		! This subroutine generates a random seed for random
		! number generation based on system time.
		!
		! call init_random_seed()
		!--------------------------------------------------
		implicit none
		
		integer 				:: i, n, clock
		integer,dimension(:),allocatable 	:: seed
		
		call random_seed(size=n)
		allocate(seed(n))
		
		call system_clock(count=clock)
		seed = clock + 37 * (/ (i - 1, i = 1, n) /)
		call random_seed(put=seed)
		
		deallocate(seed)
		
	end subroutine init_random_seed
	
end module mod_random
