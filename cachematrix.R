## These two functions together set up a way to save a matrix in cache along
## with its inverse matrix.

## This function creates a vector of 4 functions. 
###  The first function 'set' saves the given matrix in cache. 
###  The second function 'get' returns the cached matrix.
###  The third function 'setinverse' saves the inverse in cache.
###  The fourth function 'getinverse' return the cached inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## This function check if there is a value in the cache for the inverse.
### If inverse exists in cache, return the cached data for the inverse.
### Otherwise, calculate the inverse of the given matrix 
###    and save the value in cache and also return newly calculated inverse.

cacheSolve <- function(x, ...) {
	 inv <- x$getinverse()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
	 data <- x$get()
       inv <- solve(data, ...)
       x$setinverse(inv)
       inv
        ## Return a matrix that is the inverse of 'x'
}
