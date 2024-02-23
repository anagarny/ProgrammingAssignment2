## The first function, makeCacheMatrix creates a special "matrix" object, which is really a matrix created from a given set of values, get the values of the matrix, 
## set the values of the matrix inverse and get the values of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        set <- function(y = matrix()) {
                x <<- y
                m <<- NULL 
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve will calculate the inverse of a Matrix returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.

CacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
