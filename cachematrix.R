## function makeCacheMatrix(x=matrix()) solves the inverse of a matrix x and 
## stores the results in the cache
## function cacheSolve(x), accepts a matrix, searches if the matrix already has
## an inverse solved and stored in cache. If there is a cached inverse, it returns
## the cached value, else it solves for inverse of x and returns it.

## Solve for inverse of given matrix and cache the results

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function(m) m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Retreive cached inverse of a matrix if available, else use solve function to
## to get the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- getinv(x)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- get(x)
  m <- solve(data, ...)
  x$setinv(m)
  m
}
