##The following two functions work together. The first function uses the details of a matrix to store the inverse of the matrix in the cache. The second function can be used to retrieve the inverse from the cache.

## The makecacheMatrix function:
        ## The makecacheMatrix function creates a special matrix, which is really a list containing a function to:

makecacheMatrix <- function(x = matrix()) { ##1. Get the value of the matix
        m <- NULL
        setmatrix <- function(y) { ##2. Set the value of the matrix
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve  ##3. Set the value of the inverse
        getinverse <- function() m  ##4. Get the value of the inverse
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
