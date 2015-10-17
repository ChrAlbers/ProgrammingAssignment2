## This is my attempt at programming assignment 2 of the course 
## R Programming
##
## There are two functions defined here.
## The structure in general is the same as the one given by the example
## functions in the programming assignment.

## makeCacheMatrix takes a square matrix and outputs a list
## of names that refer to functions defined inside. These functions are
## set, get, setinv, getinv, which do:
## set: Sets the stored matrix
## get: Outputs the stored matrix
## setinv: Sets the inverse of the stored matrix
## getinv: Outputs the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    # inital test for matrix dimensions. Only square matrices
    # (num_rows == num_cols) can be inverted!
    if(dim(x)[1] != dim(x)[2]) {
        message("The matrix is not square and can not be inverted!")
        return(x) # return the matrix if test failed.
    }
    
    # From here the code is in principle the same as in vector mean example
    invm <- NULL # default value of the inverse; NULL
    
    # The function set can be used to change the stored matrix after
    # the first call of makeCachedMatrix
    set <- function(y) {
        x <<- y
        invm <<- NULL # in case of changing matrix, set stored inv to default
    }
    # The function get is used to access the stored matrix
    get <- function() x
    # setinv sets the stored inverse
    setinv <- function(inv) invm <<- inv
    # getinv outputs the stored inverse
    getinv <- function() invm
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve is tailored to work on the list
## output by makeCacheMatrix. As argument it takes the list, accesses
## the stored inverse via getinv, and in case the inverse is NULL
## (default value), cacheSolve computes the inverse and stores it
## inside makeCacheMatrix by calling setinv

cacheSolve <- function(x, ...) {
    inv <- x$getinv() # get the stored inverse
    if(!is.null(invm2)) {
        message("Using cached inverse")
        return(inv) # return stored inverse in case not NULL
    }
    # The inverse of a matrix is computed with solve()
    x$setinv(solve(x$get())) # saves two lines of code compared to example
    x$getinv()
}



