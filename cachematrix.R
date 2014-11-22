## This script takes a matrix and finds it inverse; if computation has already
## been performed, it returns the calculated value

## makeCacheMAtrix() takes a matrix and sets/gets its value and inverse

## define function, input is will be a matrix
makeCacheMatrix <- function(x = matrix()) {
    ## m will be our inverse, resets to NULL each time called
    m <- NULL
        ## takes an input matrix
        set <- function(y) {
                ## saves the input matrix
                x <<- y
                ## reset inverse to NULL
                m <<- NULL
        }
        ## returns value of original matrix
        get <- function() x
        ## called by cacheSolve() during first run, stores using superassigment
        setinverse <- function(solve) m <<- solve
        ## returns cached value to cacheSolve() on subsequent access
        getinverse <- function() m
        ## list of all internal functions for calling function reference
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() finds inverse of matrix created in above function and checks to
## see if the inverse has already been calculated; if so, returns inverse, if
## not, calculates it

## define function, input is object created by makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## access the object, get inverse
    m <- x$getinverse()
        ## if inverse was cached already...
        if(!is.null(m)) {
                ## print message
                message("getting cached data")
                ## returned cached value
                return(m)
        }
        ## otherwise...
        ## assign original matrix to variable 'data'
        data <- x$get()
        ## assign inverse of data to m
        m <- solve(data, ...)
        ## stores inverse value in x
        x$setinverse(m)
        ## returns inverse to code called by function
        m
}