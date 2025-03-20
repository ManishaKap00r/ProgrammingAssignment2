##Here we have defined two functions: makeCacheMatrix and cacheSolve. makeCacheMatrix takes
##a matrix argument and returns an object with a list of functions to get, set the matrix
##and get and set the matrix inverse. cacheSolve takes an argument, which is an object created by
##makeCacheMatrix function and calculates inverse of the matrix. If the inverse has been 
##previously calculated and cached, it retrieves the cached result.



## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## This function returns an object with a list of functions to:
## Set and get the matrix.
## Set and get the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) matrix_inverse <<- inverse
  get_inverse <- function() matrix_inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}



## cacheSolve function calculates the inverse of the matrix stored in the cache object.
## If the inverse has been previously calculated and cached, it retrieves the cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        matrix_inverse <- x$get_inverse()  # Get cached inverse
        if(!is.null(matrix_inverse)) {
          message("getting cached inverse of matrix")
          return(matrix_inverse)
        }
        matrix <- x$get()  # Get the original matrix
        
        # Check if the matrix is empty
        if (length(matrix) == 0) {
          message("Matrix is empty")
          return(NULL)
        }
        
        # Check if the matrix is square
        if (nrow(matrix) != ncol(matrix)) {
          message("Matrix is not square")
          return(NULL)
        }
        
        # Check if the matrix is invertible (determinant is non-zero)
        if (det(matrix) == 0) {
          message("Matrix is square but not invertible (determinant is zero)")
          return(NULL)
        }
        
        # Compute the inverse if the matrix is valid
        matrix_inverse <- solve(matrix, ...)  # Compute the inverse
        x$set_inverse(matrix_inverse)  # Cache the inverse
        
        # Return the computed inverse
        matrix_inverse
}

