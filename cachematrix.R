## CachedMatrix Creation
## Ashish Verma May-19



makeCacheMatrix <- function(x = matrix()) {
  
  indicator<-NULL
  set<-function(y){
    x<<-y
    indicator<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) indicator <<- inverse
  getinverse <- function() indicator
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  value <- x$getinverse()
  if (!is.null(value)) {
    message("getting cached data")
    return(value)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(value)
  value
  
}