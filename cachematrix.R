makeCacheMatrix <- function(x = matrix()) {
  ## Caching the matrix and its inverse
  m <- NULL  ## m will be used to define the inverse
  set <- function(y) {  
    x <<- y     ## defining the matrix by the input
    m <<- NULL  ## resetting m
  }
  get <- function() x  ## calling the matrix from  cache-memory
  setinverse <- function(inverse) m <<- inverse  ## defining the inverse
  getinverse <- function() m ## calling the inverse from cache-memory
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() ## defining the inverse
  if(!is.null(m)) {                 ## if the inverse has succesfully been aquired 
    message("getting cached data")  ## send message and return the inverse
    return(m)
  }
  data <- x$get()   ## calling the matrix
  m <- solve(data, ...) ## calculating the inverse
  x$setinverse(m)   ## defining the inverse
  m     ## returning the inverse
}
