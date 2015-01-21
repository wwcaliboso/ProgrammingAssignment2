## The first function, makeCacheMatrix creates a matrix, 
##  which is a list containing function to
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the inverse of the matrix
##    4. get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) s <<- solve
  getInverse <- function() s
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the inverse of the matrix created
##  above. It checks first to see if the inverse has already been calculated.
##  If so, it gets the inverse from the cache and skips the computation.
##  Otherwise, it calculates the inverse of the matrix and sets the value of the
## inverse in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getInverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setInverse(s)
  s
}
