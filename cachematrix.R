makeCacheMatrix <- function(x = numeric()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inverse <<- solve
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(m1, ...) {
  inverse <- m1$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- m1$get()
  inverse <- solve(data, ...)
  m1$setinv(inverse)
  inverse
}