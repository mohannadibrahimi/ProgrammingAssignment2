## Caching the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {

    cache <- NULL
    set <- function(y) {
	
        x <<- y
        cache <<- NULL
		
    }
	
    get <- function() x
    setinverse <- function(inverse) cache <<- inverse
    getinverse <- function() cache
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
	
}

## `cacheSolve` computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#If the inverse has already been calculated, then`cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {

    cache <- x$getinverse()
    if(!is.null(cache)) {
	
        message("Retrieving cached data")
        return(cache)
		
    }
    data <- x$get()
    cache <- solve(data)
    x$setinverse(cache)
    cache
	
}
