
## The pair of functions implement a Matrix which can cache its inverse.
## The makeCacheMatrix(m) function



## The makeCacheMatrix(m) function initiallises a matrix which can cache its own inverse
## Usage:
## Makes basic matrix m
## m <- matrix(1:4, nrow=2, ncol=2) 
## Make caching version of matrix, M
## M <- makeCacheMatrix(m)
## The cacheSolve(M) function can then be used to calculate the inverse
## M$getInverse() can be used to access the inverse once it is calculated.
makeCacheMatrix <- function(x = matrix()) {
  # Set inverse initially to be NULL
  xi <- NULL
  
  # Define function that takes a matrix y:
  # - assigns it to x in the outer environment
  # - initiallises the inverse as NULL
  set <- function(y) {
    x <<- y
    xi <<- NULL
  }
  
  # Define function that needs no args and just returns the matrix x
  get <- function() x
  
  # Define function that takes the inverse matrix and assigns it to xi in the scope 
  # of the main function
  setInverse <- function(i) {
    xi <<- i
  }
  # Define function that needs no args and just returns the inverse of x
  getInverse <- function () xi
  
  
  # Return a list, where each item is named with the name of one of the functions
  # and where the items themselves are the functions to operate the cache
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}



## The cacheSolve(M) function finds the inverse of the special matrix M
## The matrix M must be created with makeCacheMatrix(m) from a normal matrix m
## cacheSolve(M) can be called repeatedly, but will only do the calculation once
## of the inverse.  The inverse will be stored within M.
cacheSolve <- function(x, ...) {
  # First try and get the cached inverse
  xi <- x$getInverse()
  
  # Check if the inverse is null
  if (is.null(xi)) {
    # Get the matrix
    m <- x$get()
    # Find the inverse and store within this function
    xi <- solve(m)
    # Store inverse within the caching object
    x$setInverse(xi)
  }
  else
  {
    # If not, then will just return it, so just put up message
    message("Returning cached inverse")
  }
  # Return the inverse, which is either from the cache, or freshly calculated
  return(xi)
}







# ===============================================================
# Testing

# Create a simple matrix
m <- matrix(1:4, nrow=2, ncol=2)
# Check that inverse exists
solve(m)

# Make the caching version of the matrix
M <- makeCacheMatrix(m)
# Check can get the value
M$get()
# Show that currently no inverse available
M$getInverse()

# Get the inverse and at the same time cache it in M
cacheSolve(M)
# Get inverse again, and notice message saying it is cached
cacheSolve(M)
# Show that inverse is stored in M
M$getInverse()

