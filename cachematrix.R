## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## x       a square invertible matrix
    ## return  a list containing functions to
    ##              1. set the matrix
    ##              2. get the matrix
    ##              3. set the inverse
    ##              4. get the inverse
    ##         this list is the input to cacheSolve()
    
    inv = NULL
    set = function(y) {
        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## Compute the value of the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of x
    ## x        output of makeCacheMatrix()
    ## return   inverse of the original matrix created with makeCacheMatrix()
    
    inv = x$getinv()
    
    # check to see if the inverse has already been calculated
    if (!is.null(inv)){
        return(inv)
    }
    
    # if not, calculates the inverse 
    mat.data = x$get()
    inv = solve(mat.data, ...)
    
    # set the value of the inverse 
    x$setinv(inv)
    
    return(inv)
}
