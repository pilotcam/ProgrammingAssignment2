## Programming Assignment 2:  Cachine the Inverse of a Matrix
## These fuctions are used to calculate and cache the inverse of
## a matrix

## Generates an object to store a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function (y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) cache <<- inv
  getinverse <- function() cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the matrix.  If the
## inverse has been previously calculated, the cached value
## will be returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Cached inverse")
    return(inv)
  }
  m <- x$get()
  m_inv <- solve(m)
  x$setinverse(m_inv)
  m_inv
}
