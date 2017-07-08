makecachematrix <- function( x = matrix()){
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) Inv <<- inverse
    getInverse <- function() Inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
    
}

cachesolve <- function(x, ...) {
    Inv <- x$getInverse()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInverse(Inv)
    Inv
}

