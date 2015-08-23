makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL 
  #function set for set a new matrix
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  #function get for get a current matrix
  get <- function() x
  
  #function setinverse for set a new inverse matrix
  setinverse <- function(inverse) cache <<- inverse
  
  #function getinverse for get a cache matrix  
  getinverse <- function() cache
  
  #return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  #get cache matrix
  cache <- x$getinverse()
  
  #if cache not null-matrix
  if(!is.null(cache)) {
    #print, that we get result from cache
    message("getting cached data")
    return(cache)
  }
  
  #get matrix
  data <- x$get()
  #get inverse matrix, using function solve()
  cache <- solve(data, ...)
  #set inverse matrix to cache
  x$setinverse(cache)
  cache
}
