## These functions were made for preventing excessive calculation in inversing matrix
## It cache inversed matrix and won't inverse it again, just return cached value


## It make list with get/set functions with captured state which has two matrix: orignal and inversed  
## inversed set to NULL, it will get value when happened first access to inversed matrix with function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    
    set <- function(new_m) {
        m <<- new_m
        solved <<- NULL
    }
    
    set(x)
    
    get <- function() m
    
    setinverse <- function(inverse) solved <<- inverse
    
    getinverse <- function() solved
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x' and cache the inversed value
## Next request will return cached value without any calculations

cacheSolve <- function(x, ...) {
    
    ret <- x$getinverse()

    if(!is.null(ret)) {
        message("getting cached data")
        return(ret)
    }    
    
    ret <- solve(x$get(), ...)
    
    x$setinverse(ret)
    
    ret
}