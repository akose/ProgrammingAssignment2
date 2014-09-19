## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# creates a matrix that will be inverted later in cacheSolve

makeCacheMatrix <- function(x = matrix())
{
  #Internal functions
  #Put these first so you can find all of the functions
  Set_Matrix <- function(Data)
  {
    # <<- is used to assign value that is outside of the current environment
    Inverse <<- Inverse
    M <<-Data
  }
  Get_Matrix <- function()
  {
    M
  }
  Set_Inverse <- function(inverse)
  {
    Inverse <<- inverse
  }
  Get_Inverse <- function()
  {
    Inverse
  }
  #End of Internal Function setup

  #Creating NULL vectors
  M <- NULL
  Inverse <- NULL
  #Initialize Data
  Set_Matrix(x)
  #List of internal functions
  list(Set_Matrix = Set_Matrix,
       Get_Matrix = Get_Matrix,
       Set_Inverse = Set_Inverse,
       Get_Inverse = Get_Inverse)

}



## Write a short comment describing this function
# inverts the matrix supplied to makeCacheMatrix if already calculated return cached answer
# Assume that the matrix sent is invertible
# Note if matrix sent is not invertible 
# This type of error occurs in this instance a 3x3 matrix
#
# Error in solve.default(a = M) : 
# Lapack routine dgesv: system is exactly singular: U[3,3] = 0 
#
#
cacheSolve <- function(x=matrix(), ...)
{
  ## Return a matrix that is the inverse of 'x'
  Inverse <- x$Get_Inverse()
  # If Inverse is still a NULL vector !is.null(Inverse) equals FALSE and is skipped
  if(!is.null(Inverse)) {
    message("getting cached data.")
    return(Inverse)
  }
  M <- x$Get_Matrix()
  Inverse <- solve(a = M)
  # sets the Inverse to what was returned from solve(a=M)
  x$Set_Inverse(Inverse)
  message("This is the matrix")
  print(M)
  message("This is the inverse of that matrix")
  print(Inverse)
  message("If this is an identity matrix it worked correctly")
  # using %*% multiplies the matrices together if you just use * it will multiply cell by cell
  print(M%*%Inverse)
}

# Example Output
#Source cachematrix.R
#> a <- makeCacheMatrix(matrix(c(2,0,2,0,1,2,0,0,2),ncol=3,nrow=3))
#> cacheSolve(a)
#This is the matrix
#[,1] [,2] [,3]
#[1,]    2    0    0
#[2,]    0    1    0
#[3,]    2    2    2
#This is the inverse of that matrix
#[,1] [,2] [,3]
#[1,]  0.5    0  0.0
#[2,]  0.0    1  0.0
#[3,] -0.5   -1  0.5
#If this is an identity matrix it worked correctly
#[,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1
#> cacheSolve(a)
#getting cached data.
#[,1] [,2] [,3]
#[1,]  0.5    0  0.0
#[2,]  0.0    1  0.0
#[3,] -0.5   -1  0.5
