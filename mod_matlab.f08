module mod_matlab
	!--------------------------------------------------
	! This module is for re-creating a few useful Matlab commands in Fortran.
	! This module utilizes LAPACK/BLAS subroutines and must be included in the compile!
	!
	!**** SUMMARY *****
	! Some of the Fortran implementations do not act exactly like the Matlab equivalent.
	! Take care to check what the outputs of the subroutines and functions are,
	! they are denoted with '_' after variable name for a vector output, '_,_' for a matrix output.
	! Subroutines are denoted with 'SR' and functions are denoted with 'FU'
	!
	! Matlab Function		Fortran equivalent
	! linspace				SR: linspace( a , b , n , y_ )
	! save						SR: save_mat( FileName , mat )
	! read						SR: read_mat( FileName , mat , m , n)
	! polyfit					SR: polyfit( x , y , n , c_ )
	! polyval					FU: polyval( x , c )
	! hist						SR: histogram( x , n , hist_ , bin_space_ )
	! mean						FU: mean(x)
	! stdev						FU: stdev(x)
	!--------------------------------------------------
	implicit none
	contains
	
	subroutine linspace(a,b,n,y)
		!--------------------------------------------------
		! Linspace creates a vector of equally spaced points
		! of a specified length.
		!
		! call linspace(a,b,n,y)
		! a	Start point
		! b	End point
		! n	Number of points
		! y	Output vector (length(n)
		!--------------------------------------------------
		implicit none
		
		real,intent(in)					::	a,b
		integer,intent(in)				::	n
		real,dimension(n),intent(out)	::	y
		integer		::	i
		
		! Check if n>0
		if (n .lt. 1) then
			print *, 'linspace(): N must be an integer greater than 1!'
			return
		end if
		
		do i=1,n
			y(i) = a + (i-1)*(b-a)/(n-1)
		end do
		
	end subroutine linspace
	
	
	subroutine save_mat(FileName,mat)
		!--------------------------------------------------
		! This subroutine saves a matrix 'mat' to a file 'FileName'
		!--------------------------------------------------
		implicit none
		
		character(len=*),intent(in)	::	FileName
		real,intent(in)				::	mat(:,:)
		integer						::	i
		
		! Open the file and write to it line by line
		open (unit = 10, file = FileName)
		do i=1,size(mat,1)
			write(10,*), mat(i,:)
		end do 
		
	end subroutine save_mat
	
	
	subroutine read_mat(FileName,mat,m,n)
		!--------------------------------------------------
		! This subroutine reads a matrix from 
		! 'FileName' to a matrix 'mat'
		!
		! call read_mat(FileName,mat,m,n)
		! FileName		Name of the file with matrix
		! mat			Values will be read into this matrix
		! m				Number of rows in matrix
		! n				Number of columns in matrix
		!--------------------------------------------------
		implicit none
		
		integer,intent(in)			::	m,n
		character(len=*),intent(in)	::	FileName
		real(kind(0.0d0)),intent(out)	::	mat(m,n)
		integer						::	i,j
		
		open(unit = 10, file = FileName)
		do i=1,m
			read(10,*) (mat(i,j),j=1,n)
		end do
		
	end subroutine read_mat
	
	
	subroutine polyfit(x,y,n,c)
		!--------------------------------------------------
		! This subroutine returns the coefficients 'c' of the
		! 'k' degree polynomial for the vectors 'x' and 'y'.
		! Optionally, the rms error can be calculated as well.
		!
		! call polyfit(x,y,n,c,rms)
		! x		x data for fitting
		! y		y data for fitting
		! n		Degree of polynomial for the fit
		! c		Coefficients of polynomial fit
		!--------------------------------------------------
		implicit none
		
		integer,intent(in)						:: n
		real,dimension(:),intent(in)	:: x, y
		real,intent(out)							:: c(:)
		
		! Local variables
		integer	 							:: i1, i2, m
		real,allocatable			:: VM(:,:)
		
		! Local sgels variables
		character,parameter		::	trans = 'N'
		integer								::	info, lda, ldb, lwork
		real									::	work(100)
		
		! Check if x and y data are the same length
		if (size(x) .ne. size(y)) then
			print *, 'Polyfit(): x and y data are not the same length!'
			return
		end if
		
		m	=	size(x)
		lda	= 	max(1 , m)
		ldb	= 	max(1 , max(m , n+1))
		allocate(VM(m,n+1))
		
		! set up  Vandermonde matrix
		do i1=1,m
			do i2=0,n
				VM(i1,i2+1) = x(i1)**(real(i2))
			end do
		end do
		
		! query for optimal workspace
		lwork = -1
		call sgels( trans, m, n+1, 1, VM, lda, y, ldb, work, lwork, info )
		lwork = min(100,int(work(1)))
		
		! Solve the equations
		call sgels( trans, m, n+1, 1, VM, lda, y, ldb, work, lwork, info )
		
		if (info .lt. 0) then
			print *, 'Failed attempt to solve equations set.'
			return
		end if
		
		! save coefficients
		c = 0.0
		c = y
		
	end subroutine polyfit
	
	real function polyval(x,c)
		!--------------------------------------------------
		! This function evaluates a polynomial with coefficients
		! 'c' at position 'x'.
		!
		! polyval(x,c)
		! x		Location at which the function is evaluated
		! c		Vector of coefficients (see polyfit)
		!--------------------------------------------------
		implicit none
		
		real,intent(in)		:: x , c(:)
		integer 					:: m , i1
		real,allocatable	:: VM(:)
		
		m = size(c) - 1
		allocate(VM(m+1))
		
		! set up  Vandermonde matrix
		do i1=0,m
			VM(i1+1) = x**(real(i1))
		end do
		
		! Evaluate function
		polyval = dot_product(VM , c)
				
	end function polyval
	
	
	subroutine histogram(x,n,hist,bin_space)
		!--------------------------------------------------
		! This subroutine computes a histogram of
		! 'x' with 'n' bins.
		!
		! Call histogram(x,n)
		! x						Data points (in)
		! n						Number of bins to use (in)
		! hist				Histogram (out)
		! bin_space		Bin domains
		!--------------------------------------------------
		implicit none
		
		! Passed in
		real, intent(in)			:: x(:)
		integer, intent(in)		:: n
		
		! local
		real, intent(out)			:: bin_space(n+1)
		integer, intent(out)	:: hist(n)
		integer								:: i,j
		
		!Create bin space
		call linspace(minval(x) , maxval(x)*1.0001 , n+1 , bin_space)
		
		hist = 0.0
		! sort data into histogram
		do i=1,size(x)
			do j=1,n
				if ( (x(i) .ge. bin_space(j)) .and. (x(i) .lt.bin_space(j+1)) ) then
					hist(j) = hist(j) + 1
					cycle
				end if
			end do
		end do
		
	end subroutine histogram

	real function mean(x)
		!--------------------------------------------------
		! Returns the mean of a 1D array
		!--------------------------------------------------
		implicit none
		
		real,intent(in) :: x(:)
		
		! If the vector size is 0, then throw error
		if (size(x) .eq. 0) then
			print *, 'mean(): Zero size vector'
			return
		end if
		
		mean = sum(x) / max(1 , size(x))
	end function mean
	
	real function stdev(x)
		!--------------------------------------------------
		! Returns the standard deviation of a 1D array
		!--------------------------------------------------
		implicit none
		
		real,intent(in) :: x(:)
		real :: mu
		
		! If the vector size is 0, then throw error
		if (size(x) .eq. 0) then
			print *, 'mean(): Zero size vector'
			return
		end if
		
		! Calculate mean
		mu = mean(x)
		
		! Calculate standard deviation
		stdev = sqrt( sum((x-mu)**2.0) / max(1 , size(x)) )
		
	end function stdev

end module mod_matlab
