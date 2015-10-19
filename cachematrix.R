## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  #the "matrix" object will have my_inv as a property, set to NULL by default
  
  my_inv <- NULL
  
  
  #the "matrix" object will have 4 properties: set() and get(), setinverse() and getinverse()
  set <- function(y) {
    x <<- y
    my_inv <<- NULL #Will ensure that inverse is recalculated in case of change in the matrix
  }
  get <- function() x
  setinverse <-function(inv) my_inv <<-inv
  getinverse <-function() my_inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    #we getinverse() and check if there is already a value in the cache
    my_inv <- x$getinverse()
    
    if(!is.null(my_inv)) {
      message("getting cached inverse matrix")
      return(my_inv)
    }
    
    #no cache value of the inverse, so we calculate the inverse with solve()
    my_matrix <- x$get()
    my_inv <- solve(my_matrix)
    
    #we assign the value of the inverse to the inverse property of the "matrix" object
    x$setinverse(my_inv)
    my_inv
  
}
