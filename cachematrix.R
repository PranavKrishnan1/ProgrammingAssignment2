
## This funtion is written to get,set,get the inverse,
##and set the inverse of the matrix

makeCacheMatrix = function(x = matrix()) {
  m = NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get = function() x
  setinverse = function(inv) m <<- inv
  getinverse = function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function calculates and returns the 
## inverse of the matrix, or returns a cached matrix.

cacheSolve <- function(j, ...) {
  m <- j$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    m
  }
  data <- j$get()
  m <- solve(data, ...)
  j$setinverse(m)
  m
}
