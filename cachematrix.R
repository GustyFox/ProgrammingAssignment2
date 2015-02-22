# makeCacheMatrix creates a function to
# Set adn get the value of the matrix
# Set and get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL   }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. 
# It checks if the inverse is already cached and do not 
# remake the whole process if it is.
# For this function we assume the input matrix is invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data.")
      return(inv)     }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
