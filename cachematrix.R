## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
makeCacheMatrix <- function( m = matrix() ) {
  
  inverseMatrix <- NULL
  
  set <- function(matrix) {
    m <<- matrix
    inverseMatrix <<- NULL
  }
  
  get <- function() m

  getInverse <- function() inverseMatrix

  setInverse <- function(inverse) inverseMatrix <<- inverse

  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  ## https://stackoverflow.com/questions/11995832/inverse-of-matrix-in-r
  m <- solve(data) %*% data
  
  x$setInverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}