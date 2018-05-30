## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function acts on a matrix objec and creates 
## four function(set, get, setinverse, getinverse) in the parent environment

makeCacheMatrix <- function(x = matrix()) {
## the data object X is initialized in the function argument, and
## the data object m is initialized below as NULL
  m <- NULL
## the set function assigns y to x in the parent environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## the get function retrieves the value of X
        get <- function() x
  ## the setinverse function assigns the object inverse to m in the parent evironment
  setinverse <- function(inverse) m <<- inverse
  ## the getinverse function retrieves the value of m
  getinverse <- function() m
  ## makes list of all 4 functiones with assigned names
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   ## first it gets the value of m from the parent enviroment and
   ## checks if it is NULL.  If it is not NULL it returns the value 
   ## of m (which is in the chace)
   ## if m is NULL, then the inverse of this matrix was not calculated
   ## before, so it calculate it using "solve", assign it to m and print it
   m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  else { 
    data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  }
}
