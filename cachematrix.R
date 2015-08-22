## This function returns a list of function and when used property will
## store the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  # Initialize
  m <- NULL
  
  # Set function
  set <- function(y) {
    # Store the matrix and empty solution
    x <<- y
    m <<- NULL
  }
  
  # Get function
  get <- function() {
    # Return the matrix stored in the main function
    x
  }
  
  # Setmatrix function
  setmatrix <- function(solve) {
    # Cache the soluton (inversed matrix)
    m <<- solve
  }
  
  # Getmatrix function
  getmatrix <- function() {
    # Return a cached solution
    m
  }
  
  # Return a list of functions
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This function will accept the output from the makeCacheMatrix
## function and will return the inverse of the initial matrix.  On 
## additional executions it will return a cached inverse.

cacheSolve <- function(x=matrix(), ...) {
  
  # Extract a cached solution (might be NULL)
  m <- x$getmatrix()
  
  # If we have a cached solution, return it
  if(!is.null(m)){
    message("getting cached data")
    return(m)
    
  }
  
  # Get the input matrix data
  data <- x$get()
  
  # Generate a solution (inverse the matrix)
  m <- solve(data, ...)
  
  # Cache it
  x$setmatrix(m)
  
  # Return it
  m
  
}
