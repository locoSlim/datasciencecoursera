## These functions create a square matrix and allow its inverse to be cached.

testCacheMatrix <- function() {
        
    zzz <- matrix( , 2, 2)
    zzz[1,1] <- 12
    zzz[1,2] <- 32
    zzz[2,1] <- 15
    zzz[2,2] <- 25
    #makeCacheMatrix(zzz)    
    cm <- makeCacheMatrix(zzz)
    cacheSolve(cm)    
    cacheSolve(cm)    
}


## This function creates a square matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setsolve <- function(solv) {i <<- solv}
    getsolve <- function() i
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}
    
## This function inverts and caches a matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getsolve()
    if(!is.null(i)) {
        message("getting cached data")
        i
    }
    else {
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
    }        
}







