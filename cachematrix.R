## Creata a function for
## caching the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##set the value of the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ##get the value of the matrix
  get <- function() x
  ##set the value of the inversed matrix
  setmatrix <- function(mtx) m <<- mtx
  ##get the value of the cached inversed matrix
  getmatrix <- function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  ## Checks whether the value is cached or not
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## stores the inverse of the matrix
  m <- solve(data, ...)
  x$setmatrix(m)
  ## returns the inversed value
  m
}
