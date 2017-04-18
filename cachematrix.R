## Together, these functions compute the inverse of a square matrix

## Creates an object (special "matrix") that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Sets and gets the value of the matrix
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ## Sets and gets the value of the inverted matrix using the solve function
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Computes the inverse of the object generated in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  
  ## Checks whether inverse matrix is cached
  m <- x$getinverse()
  if(!is.null(m)){
    print("inverse was cached!")
    return(m)
  }
  
  ## If inverse matrix is not cached, 
  ## returns a matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}