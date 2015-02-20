## Put comments here that give an overall description of what your
## functions do

## Create a cache matrix
makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) a <<- inv
    getinverse <- function() a
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

## Inverse matrix
cacheSolve <- function(x, ...) {
    a <- x$getinverse()
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    m <- x$get()
    a <- solve(m, ...)
    x$setinverse(a)
    a
}
