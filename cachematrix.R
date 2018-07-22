## Week 3 Assignment

## Creates a matrix that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { x }
  setinverse <- function(inverse) { m <<- inverse }
  getinverse <- function() { m }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Retrives inverse from cache if already solved

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
