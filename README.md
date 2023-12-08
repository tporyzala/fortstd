# fortstd
Fortran implementations of common functions.

# Libraries

## lib_kinds
Variable kind declarations.

## lib_constants
Commonly used mathematical or engineering constants.

## lib_math
Mathematical library.
### trapz
Trapezoidal integration.
### .nearly.
"Nearly" operator for real value equality checking.

## lib_linalg
Linear algebra library.
### eye
Identity matrix.
### diag
Create diagonal matrix.
### cross
Vector cross product
### inv_2x2, inv_3x3, inv_4x4
Inverse of 2x2, 3x3 or 4x4 matrices.
### chol
Cholesy factorization of for Hermitian, positive-definite matrices.
### plu
LU factorization with partial-pivoting
### forward_solve, backward_solve
Forward and backwards equation solving after factorization.
### swap
Swap two values.

## lib_array
Array operations library
### linspace
Linearly spaced vector from a to b with n points
### logspace
Logarithmicly space vector from base^a to base^b with n points.
### arange
Vector from a to b with spacing s.
### diff
Difference between adjacent values in a vector or matrix along specified dimension.
### unique
Unique values in a vector or matrix along a specified dimension.

## lib_statistics
Statistical library.
### mean
Mean value of a vector or matrix along a specified dimension.
### std
Standard deviation of a vector or matrix along a specified dimension.

## lib_random
Random number library.
