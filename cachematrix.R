## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv = NULL
      set = function(y) {
            
            x <<- y
            inv <<- NULL
      }
      get = function()x
      setinv = function(inverse) inv <<- inverse
      getinv = function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## a square invertible matrix
## return a list containing a function to:
      ##  set the matrix
      ##  get the matrix
      ##  set the inverse
      ##  get the inverse
## the list is used as input to cacheSolve()


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      inv <- x$getinv()
      
      
      #if the inverse has already been calculated
      if(!is.null(inv)) {
            # get from cache and skips the calculation
            message("getting chached data")
            return(inv)
      }
      
      #calculates inverse if not previously done and stored
      data <- x$get()
      inv = solve(data, ...)
      
      #sets value of the inverse in cache through the setinv function
      x$setinv(inv)
      
      return(inv)
}

