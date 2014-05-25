## The first function gets the matrix, 
## inverses it and caches matrix inverse.
## As a result the function gives list of 
## containing functions.

makeCacheMatrix <- function(x = matrix()){
        m <- NULL        
        get <- function() x
        q <- solve(x)
        m <<- q
        setsolve <- function(solve)  m <<- solve  
        getsolve <- function() m
        list(get = get, 
             getsolve = getsolve,
             setsolve = setsolve)
}


## This function checks if there has already been
## compute  the matrix inverse and if so,
## the function gives the inverse from cache.
## If not it inverses the matrix.
## Finally there is set the solve and print 
## the inverse matrix.

cacheSolve <- function(x, ...) {
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
