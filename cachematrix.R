# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#             If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse
#             from the cache.


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. Make a matrix by passing
# matrix you want to find inverse for to thsi function. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

# Call this function with desired matrix to find the inverse. It will return a message if you are selecting cached data
cacheSolve <- function(x, ...) {
  message("getting in")
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}
