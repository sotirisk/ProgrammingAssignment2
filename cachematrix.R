## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        #it first checks to see if the inverse has already been calculated
        if(!is.null(m)) {
        				#If so, it gets the inverse from the cache and skips the computation
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        # Otherwise, it calculates the inverse of the data with the solve function and sets the value of the inverse in the cache via the setinverse function
        m <- solve(data, ...)
        x$setinverse(m)
        m
## m is a matrix that is the inverse of 'x'
}
