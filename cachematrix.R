##This function creates a special "matrix" object that can cache its inverse.

## Create the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## create the environment to store the inverse values
        inv <- NULL  
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        ## set the variables
        
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse= setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special"matrix" returned by 
##` makeCacheMatrix`. If the inverse has already been calculated 
## (and the matrix has not changed), then this function will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## return the cached matrix
        
        inv <- x$getinverse()
        ## check if there are cached values
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ## computing the inverse
        
        inv <- solve(data, ...)
        
        ## cache the inverse
        
        x$setinverse(inv)
        
        inv
}
