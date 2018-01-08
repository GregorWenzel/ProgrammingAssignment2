## Programming Assignment 2: Lexical Scoping

## Creates a special matrix with a cacheable inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse matrix of special matrix x
## if NULL, or returns the cached inverse matrix

cacheSolve <- function(x, ...) {
  result <- x$getInverse()
  if (!is.null(result)) {
    message("accessing cached data")
    return(result)
  }
  data <- x$get()
  result <- solve(data)
  x$setInverse(result)
  result
}
