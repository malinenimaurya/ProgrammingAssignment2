## The first function, makeCacheMatrix creates a special "vector", which is really
#a list containing a function to set the value of the vector,get the value of the vector,
#set the value of the inverse,get the value of the inverse and the second function 
# checks to see if the inverse is already calculated. If so, it gets the inverse
#from the cache and skips the computation. Otherwise, it calculates the inverse of 
#the data and sets the value of the inverse in the cache via the setinverse function 


## Creates a list of functions set,get,setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##Returns the inverse of the matrix from cache if not empty or by calculating
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
