## Functions to calculate and cache the inverse of a matrix

## Takes a numeric matrix x and wraps it in an object with
#  4 functions.
#  set() - set x to a different matrix
#  get() - return the raw matrix data
#  setInv() - cache the inverse of x
#  getInv() - return cached inverse of x

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(setX) {
    x <<- setX
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(newInv) inv <<- newInv
  
  getInv <- function() inv
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)  
}


## Takes a makeCacheMatrix object x and calculates
#  the inverse of the wrapped matrix
#  Check to see if the inverse has been cached.
#  If so, return the cached inverse; otherwise
#  calculate the inverse and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInv()
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  rawMatrix <- x$get()
  inverse <- solve(rawMatrix)
  x$setInv(inverse)
  inverse
}
