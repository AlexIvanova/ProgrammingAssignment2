# makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(sol) s <<- sol
        getSolve <- function() s
        list(set = set, get = get, 
             setSolve = setSolve,
             getSolve = getSolve)
}

# cacheSolve computes the inverse; if the inverse's been already calculated,
# then the cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
