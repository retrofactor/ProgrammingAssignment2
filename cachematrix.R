## makeCacheMatrix(x) 
##  Creates a cache for a matrix x and its inverse.
##
##  Returns a list of four functions that are used to access and modify the cached matrices:
##    get() - that retrieves the cached matrix
##    set(y) - that replaces the matirx in cache with the matirx y
##    getinv() - that retrieves the inverse of the cached matrix
##    setinv(inv) - that replaces the inverse of the cached matrix with the matrix inv

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # placeholder for the inverse of x
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<-inv
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve(x)
##  Computes the inverse of a "matrix" x (which has to be constructed 
##  using makeCacheMatrix function).
##  Returns the cached inverse if it has already been computed by the previous 
##  cacheSolve call.
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        # getting cached data
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}