## The next two functions calculate and store in cache the inverse of a square
## matrix. It only works for matrices that have an inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## If the inverse of the matrix has already been calculated it returns the
## cache data, if not it calculates and store in the cache

cacheSolve <- function(x, ...) {
     ## Return the inverse of the matrix
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setinverse(m)
     m
}
