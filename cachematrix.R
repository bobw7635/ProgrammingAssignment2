## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
    ## create a matrix that can cache its inverse
    
    inv <- NULL
    set <- function(y) {
        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse 
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
    ## Compute the value of the inverse of the matrix
    
    inv <- x$getinv()
    
    # check to see if the inverse has already been calculated
    if (!is.null(inv)){
        return(inv)
    }
    
    # if not, calculates the inverse 
    mat.data <- x$get()
    inv <- solve(mat.data, ...)
    
    # set the value of the inverse 
    x$setinv(inv)
    
    return(inv)
}
