## These functions allow you to cache the inverse of a matrix 
## and retrieve the cache when required 
## If there is no cache value the inverse of a matrix will be calculated


## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL
        set <- function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(inverse) invmatrix <<- inverse
        getinvmatrix <- function() invmatrix
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above 
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinvmatrix()
        if(!is.null(invmatrix)) {
                message("getting cached data")
                return(invmatrix)
        }
        
        data <- x$get()
        invmatrix <- solve(data, ...)
        x$setinvmatrix(invmatrix)
        invmatrix
}
