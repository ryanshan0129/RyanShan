## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL        #Set the inverse to NULL
    set <- function(y) {        #Define a function to set the x to y
        x <<- y
        Inv <<- NULL
    }
    get <- function() x        # x return into get()
    setInverse <- function(inverse) Inv <<- inverse
    getInverse <- function() Inv           #return the inverse of matrix
    list(set = set,               #List all the matrix
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inv <- x$getInverse()
    if(!is.null(Inv)) {             #Check if the inverse is already in the cache
        message("getting cached data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)        # if not, calculate the inverse of the matrix
    x$setInverse(Inv)
    Inv
}
