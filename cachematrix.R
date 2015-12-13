## This is my attempt at programming assignment 2 of the course 
## R Programming
##

## makeCacheMatrix takes a square matrix and outputs a list
## of names that refer to functions defined inside. These functions are
## set, get, setinv, getinv.
## Usage:
## Convert a matrix to cached matrix:
## mat <- makeCachedMatrix(mat)
##
## mat$get()    - Output stored matrix
## mat$set()    - Store a new matrix in mat (erases previous matrix)
## mat$getinv() - Output cached inverse. Is NULL if cacheSolve has not
##                been called before
## 

makeCacheMatrix <- function(x = matrix()) {

    # inital test for matrix dimensions (is square matrix?)
    if(dim(x)[1] != dim(x)[2]) {
        message("The matrix is not square and can not be inverted!")
        return(x) # return the matrix if it is non-square (and non-invertible)
    }
    
    invm <- NULL # default value of the inverse; NULL
    
    set <- function(y) { # Store new matrix
        x <<- y
        invm <<- NULL # in case of changing matrix, set inverse to default
    }
    get <- function() x # simply return stored matrix x
    setinv <- function(inv) invm <<- inv # store new inverse inv as invm
    getinv <- function() invm # simply return stored inverse invm
    
    # return list of function as names
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve takes as argument the list output by makeCacheMatrix.
## It outputs the inverse. On first call it computes the inverse using
## solve, in all subsequent calls it returns the cached inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinv() # get the stored inverse
    if(!is.null(inv)) {
        message("Using cached inverse")
        return(inv) # return stored inverse in case not NULL
    }
    # The inverse of a matrix is computed with solve()
    x$setinv(solve(x$get(), ...)) # saves two lines of code compared to example
    x$getinv()
}



