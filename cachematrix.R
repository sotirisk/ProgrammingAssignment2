## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #implemetation of the function that sets the cached value 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #implemetation of the function that retrieves the cached value 
        get <- function() x
        #implemetation of the function that sets the inverse
        setinverse <- function(inverse) m <<- inverse
        #implemetation of the function that gets the inverse
        getinverse <- function() m
        #the list returned contains the functions above
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
        # Otherwise, it calculates the inverse of the data with the solve function and sets the value of the inverse in the cache via the setinverse function
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
## m is a matrix that is the inverse of 'x'
}



"code output 
x = array(c(7,3,-2,5),c(2,2))
> x
     [,1] [,2]
[1,]    7   -2
[2,]    3    5
> 
> listobj <- makeCacheMatrix (x)
> cacheSolve(listobj)
            [,1]       [,2]
[1,]  0.12195122 0.04878049
[2,] -0.07317073 0.17073171
> cacheSolve(listobj)
getting cached data
            [,1]       [,2]
[1,]  0.12195122 0.04878049
[2,] -0.07317073 0.17073171
"

