##The following two functions work together. The first function takes the inverse of a matrix and caches it. The second function can be used to retrieve the inverse from the cache.

## The makecacheMatrix function:
        ## The makecacheMatrix function creates a special matrix, which is really a list containing a function to:
        ##1. Set the value of the matrix
        ##2. Get the value of the matix
        ##3. Set the value of the inverse
        ##4. Get the value of the inverse

makecacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cachesolve function:
        ## The cachesolve function calculates the inverse of the special matrix created with the makecacheMatrix function above. It first checks if the inverse has already been calculated, and if so, retrieves it from the cache and skips the computation. Otherwise, it calculates the inverse and stores it in the cache using the setinverse function.

cachesolve <- function(x=matrix(), ...) {
        m <- x$getinverse()
        if(!is.null(m)) { ## If already calculated, the inverse will be retrieved from the cache...
                message("getting cached data")
                return(m)
        } ##Otherwise...
        data <- x$getmatrix() ##The inverse of the matrix is calculated...
        m <- solve(data, ...)
        x$setinverse(m) ### Stored in the cache...
        m ## And displayed.
}
