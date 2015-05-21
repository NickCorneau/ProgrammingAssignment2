## Functions to create a cached matrix and avoid
## unnecessary re-computation of the matrice's inverse


# Function to make a cached matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # reset the m variable for every new matrix
  m <- NULL
  
  # Getters & Setters functions
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) x <<- inv
  getinv <- function() x
  
  # list enumeration of functions(attributes) available
  # for a cached matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# return the inverse of a cached matrix
cacheSolve <- function(x, ...) {
  
  # verify if 'x' inverse has been calculated (ie. get() would not equal getinv())
  if (all(x$get() == x$getinv())) {
    m <- NULL
  } else {
    # if inverse has been calculated, set 'm' to 'x' inverse
    m <- x$getinv()
  }

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # otherwise, there is no inverse matrix
  # store matrix in data
  data <- x$get()
  
  # compute, store, then return data's inverse 
  m <- solve(data, ...)
  x$setinv(m)
  m <- x$getinv()
  return(m) # inverse
  
  
}
