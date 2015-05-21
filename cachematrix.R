makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) x <<- inv
  getinv <- function() x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  x
        ## Return a matrix that is the inverse of 'x'
}
