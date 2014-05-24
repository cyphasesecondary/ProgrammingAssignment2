# These functions implement caching of the inverse of a matrix, so that you
# only have to compute the inverse of a given matrix once, even if you use it
# multiple times.


# makeCacheMatrix takes a matrix as an argument and returns a list of functions
# that act upon that matrix.
makeCacheMatrix <- function(mat = matrix()) {
  # We're creating a new object, so set the inverse to NULL
  inv <- NULL
  
  set <- function(m) {
    mat <<- m
    inv <<- NULL
  }
  get <- function() mat
  getinverse <- function(...) {
    # If the inverse has not been calculated
    if(is.null(inv)){
      # Calculate and store the inverse
      i <- solve(mat, ...)
      inv <<- i
    # Otherwise, if the inverse has been calculated
    } else {
      message("getting cached inverse matrix")
    }
    # Either way, at this point we can return the inverse
    inv
  }
  
  # Create and return the final list
  list(set = set,
       get = get,
       getinverse = getinverse)
}


# cacheSolve takes a special "matrix" object and returns the inverse.
# Originally, this did more work, but that code has been integrated into the
# special "matrix" object (so that everything lives in one place). Therefore,
# cacheSolve is just a thin wrapper at this point.
cacheSolve <- function(x, ...) {
  x$getinverse()
}