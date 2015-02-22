## It will calculate inverse of matrix and cache results in environment

## create a special matrix list from input matrix and store its mean
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # calcualte inverse
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Retrieve cached inverse results
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
  # Get cached inverse here
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # In case cached inverse is not there, calculate again
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}
