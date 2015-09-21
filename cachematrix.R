## makeCacheMatrix contains 4 functions that:
##      -Stores (caches) a given matrix (set)
##      -Returns the cached matrix (get)
##      -Stores the inverse of the matrix (setinverse)
##      -Returns the (cached) inverse of the matrix (getinverse)
##
## cacheSolve determines the inverse of a given matrix. It will 
## return a cached solution (if any), otherwise, it will caculate the
## inverse and store the solution using the above makeCacheMatrix functions

## makeCacheMatrix holds the basic set/get functions. No actual
## calculations done here.
makeCacheMatrix <- function(x = matrix()) {
  # start with an empty cache by setting the matrix solution
  # (the inverse, inv) to NULL
  inv <- NULL
  # set caches the matrix. It also sets the solution (inv) to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get returns the current cached matrix
  get <- function() x
  # setinverse and getinverse just stores and returns the inverse
  # of the matrix. It could actually store anything. No work done here
  # other than storage and return
  setinverse <- function(new_inv) inv <<- new_inv
  getinverse <- function() inv
  
  # needed to make the functions a list so they can be called externally
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of a given matrix.
## It uses makeCacheMatrix to store and return its solution
cacheSolve <- function(x, ...) {
  # Is there already a cached solution? If so, return it
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # otherwise, get the current matrix
  data <- x$get()
  # calculate its inverse
  inv <- solve(data, ...)
  # cache the solution
  x$setinverse(inv)
  # as well as returning it
  inv
}
