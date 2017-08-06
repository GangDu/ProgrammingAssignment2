## These two functions creat a special "matrix" and compute 
## the inverse of the special "matrix" and cache its inverse.

## This function create a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrix_inv <- NULL
    set <- function(y){
        x <<- y
        matrix_inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) matrix_inv <<- inverse
    getInverse <- function() matrix_inv
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"
## return by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
