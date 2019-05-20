## CachedMatrix Creation
## Ashish Verma May-20-2019


##Programming Assignment 2

#Function to create Cache Matrix.
makeCacheMatrix <- function(x = matrix()) {
  
  indicator<-NULL   ##Assigning NULL to indicator
  set<-function(y){
    x<<-y
    indicator<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) indicator <<- inverse ##Assigning inverse to indicator
  getinverse <- function() indicator
  list(set = set,                            ##In the list preparing the serinverse and gerinverse
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#Function to create the CacheSolve
cacheSolve <- function(x, ...) {
  value <- x$getinverse()
  if (!is.null(value)) {          ##Checking if the value are not NULL
    message("getting cached data") ##Printing the message
    return(value)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(value)
  value                ##Returning the value back to fucntion call
  
}
