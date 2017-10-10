## Some computations, as the computation of the inverse of a matrix, takes too much memory,
## as so, it's advantageous to cache the data instead of recalculating it every time.
## The function makeCacheMatrix and cacheSolve will work in that way.

## makeCacheMatrix creates a list of four functions (set(), get(), setinverse() and getinverse()))
## that will be used by the cacheSolve() function as input
## set() will set the matrix
## get() will get the matrix
## setinverse() will set the inverse of the matrix
## getinverse() will get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        xinverse <- NULL
        
        set <- function(y = matrix()){
                x <<- y
                xinverse <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inv) xinverse <<- inv
        
        getinverse <- function() xinverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Here the function solve() will be executed to retrieve the inverse of the matrix x inputed in
## makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        xinverse <- x$getinverse()
        if(!is.null(xinverse)){
                message("getting cached data")
                return(xinverse)
        }
        data <- x$get()
        xinverse <- solve(data)
        x$setinverse(xinverse)
        return(xinverse)
}

## TESTING
## > m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
##
## > m1
## [,1]  [,2]
## [1,]  0.50 -1.00
## [2,] -0.25  0.75
##
## > myMatrix_object <- makeCacheMatrix(m1)
##
## > cacheSolve(myMatrix_object)
##      [,1] [,2]
## [1,]    6    8
## [2,]    2    4