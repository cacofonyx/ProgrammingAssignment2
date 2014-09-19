## The first function, makeCacheMatrix, creates a special
## matrix, which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    xinverse <- NULL
    set <- function(m) {
        x <<- m
        xinverse <<- NULL
    }
    get <- function() x
    setinv <- function(solve) xinverse <<- solve
    getinv <- function() xinverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. If the inverse is already calculated
## for the given matrix, it is fetched from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinverse <- x$getinv()
    if(!is.null(xinverse)) {
        message("Inverse from cached variable: \n")
        return(xinverse)
    }
    newinv <- x$get()
    xinverse <- solve(newinv,...)
    cat("Inverse of the matrix: \n")
    x$setinv(xinverse)
    xinverse
}

## Sample output from the script:
## > myM <- matrix(c(4, 2, 7, 6), 2, 2)
## > myM
##   [,1] [,2]
##   [1,]    4    7
##   [2,]    2    6
## > cm <- makeCacheMatrix(myM)
## > cacheSolve(cm)
## Inverse of the matrix: 
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > cacheSolve(cm)
## Inverse from cached variable: 

##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
> 
