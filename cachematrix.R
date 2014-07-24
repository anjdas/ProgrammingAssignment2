## The makeCacheMatrix function creates a special "matrix" type object
## and caches its inverse
## The cacheSolve function computes the inverse of the special "matrix" type object
## that was created by makeCacheMatrix function. This function, however, first checks whether
## the inverse has already been calculated. If so, it simply retrieves the cached value.

## The makeCacheMatrix creates a special "matrix" object and caches its inverse
## The function is essentially a list of functions that help to:
## set the value of a matrix [set(y)]
## get the value of a matrix [get()]
## set the inverse of a matrix [setinverse(inverse)]
## get the inverse of a matrix [getinverse()]

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse =  setinverse, getinverse = getinverse)
}


## The cacheSolve function calculates and returns the inverse of the 
## special "matrix" object created using the above makeCacheMatrix
## function. Before computing the inverse, it checks whether a
## value exists in the cache. If so, it retrieves the value from the 
## cache instead of doing another computation
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ## Checking if an inverse has already been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## If no inverse exists, then calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
