## These functions will help cache the inverse of a matrix. Then depending on if 
## the inverse has already been calculated it will retrieve it. If not, it will compute the inverse.

## This first function creates a matrix that will cache the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list( set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
}


## This second function will check if the inverse of a matrix has already been calculated. 
## If it has, it will retrieve it, but if not, it will compute it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
