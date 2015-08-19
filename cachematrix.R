## caching the inverse of a matrix
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        #1.  set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        #2.  get the value of the matrix
        get <- function() x
        #3.  set the value of the inverse
        setinv <- function(solve) i <<- solve
        #4.  get the value of the inverse
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}
## `cacheSolve` function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` should retrieve the inverse from the cache.

## At the moment giving: Error in x$getinv : $ operator is invalid for atomic vectors
cachesolve <- function(x, ...) { 
        i<- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i<- solve(data, ...)
        x$setinv(i)
        i
}
