## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                   # hold values of inverse, and set as NULL
  set <- function(y) {          # define the set function to assign new 
    x <<- y                     # value of matrix 
    inv <<- NULL                # reset inv to NULL
  
  get <- function() x           # returns value of matrix
  
  setinverse <- function(inverse) inv <<- inverse  # value of inverse in parent
  getinverse <- function() inv                     # gets the value of inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  # Need to refer to the functions with the $ operator
  }
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                       # if inverse matrix is not NULL
    message("Getting Cached Values")   
    return(invMatrix)                             # return inverse matrix
  }
  
  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMatrix()                     # get original matrix
  invMatrix <- solve(MatrixData, ...)             # use solve function to inverse matrix
  x$setInverse(invMatrix)                        
  return(invMatrix)                               # return the inverse matrix
  }
