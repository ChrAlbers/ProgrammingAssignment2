# Test of the template for mean of vector provided by the R-Programming course

# I struggled. The concept of a list of functions is quite alien to me.
# I come from Matlab, where I always wrote my code as i could literally
# grasp it. Functions are totally encapsulated from the world outside except
# the arguments I would pass to them.
# Here, what makeVector does is it creates a list of functions waiting
# to be used. That is alien, since I always feel lines of code should be
# evaluated in order, the interpreter goes through it line by line as they
# are called. But here, the functions are called and retain a memory of the
# initial call. Also, after the initial call of makeVector the functions are
# called by name of the output list.

makeVector <- function(x = numeric()) {
    # m is the symbol that points to the mean of the argument x.
    # However, it is not computed during the first call. Instead,
    # it gets a placeholder value, NULL. Only later when calling cachemean
    # the function setmean is evaluated.

    m <- NULL
    # This is the only line of code that is actually executed during
    # the first call. The rest of the lines are function definitions that
    # wait for their execution from outside the makeVector function
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # The function set can be used to reset the stored vector to a new
    # value. In this case, the mean is that of the previously stored vector
    # and has to be reset again.

    get <- function() x
    # The function get is used to access the actual vector that has been
    # stored by calling makevector or x$set(<new vector>)
    
    setmean <- function(mean) m <<- mean
    # setmean is used to store a new value in the variable m. Be careful, 
    # with some criminal inten you can store anything here.
    
    getmean <- function() m
    # This function getmean returns the stored value in m
    
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
    # This is the output of the function makeVector, a list of references
    # to functions that manipulate the vector and m (that lives inside)
    # this function
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    x$setmean(mean(x$get())) # saves two lines ...
#    data <- x$get()
#    m <- mean(data, ...)
#    x$setmean(m)
    x$getmean()
}