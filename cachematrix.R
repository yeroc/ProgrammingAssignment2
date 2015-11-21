## Assignment 2: Lexical Scoping
## November 2015

## This function takes a matrix as input, turning it into a matrix
## which can be passed into the cacheSolve() function which will use
## the added functions to cache the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  # variable to store the calculated inverse matrix
  inverse <- NULL
  # set a new value to the matrix, clearing any cached inverse.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # return the matrix
  get <- function() x
  # store the inverse matrix
  setinverse <- function(inv) inverse <<- inv
  # retrieve the inverse matrix
  getinverse <- function() inverse
  
  # create & return a list to store the functions
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## Takes a cacheable matrix as input allowing a cached inverse matrix
## to be returned on subsequent calls with the same input.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
