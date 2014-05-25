


##This function creates a matrix object that can 
##cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
            x <<-y
            i <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) i <<- inverse
     getInverse <- function() i
     list(set = set, get = get, 
          setInverse = setInverse,
          getInverse = getInverse)
    
}


## This function takes the "special" matrix as an argument
##and either returns a cached inverse of the matrix or computes 
##the inverse and then returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
