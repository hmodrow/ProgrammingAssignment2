## Matrix with cache for easy inverse calculation

## Matrix wrapper for inverse calculation cache

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_x) inv_x <<- inverse_x
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Inverse calculation over a cache matrix
## If inverse was calculated get that value

cacheSolve <- function(x, ...) {
  inv_x <- x$getinverse()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setinverse(inv_x)
  inv_x
}
