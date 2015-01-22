# makeCacheMatrix is a function that returns a list of functions
# It Contains the following functions:
# * setM       set the value of a matrix
# * getM       get the value of a matrix
# * cacheInv   get the cahced value (inverse of the matrix)
# * getInv     get the cahced value (inverse of the matrix)


makeCacheMatrix <- function(x = matrix()) {
        
        
        # holds the cached value or NULL if nothing is cached
        cache <- NULL
        
        # store a matrix
        setM <- function(y) {
                x <<- y
                cache <<- NULL
        }

        # returns the stored matrix
        getM <- function()    x
        

        # cache the given argument 
        cacheInv <- function(z)    cache <<- z

        # get the cached value
        getInv <- function()  cache
        
        # return a list. Each named element of the list is a function
        list(setM = setM, getM = getM, cacheInv = cacheInv, getInv = getInv)
        
}


##cacheSolve calculates inverse of a special matrix created with above fuction

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inverse <- x$getInv()
        # if a cached value exists return it
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        # otherwise get the matrix, caclulate the inverse and store it in the cache
        data <- x$getM()
        cache <- solve(data)
        x$cacheInv(cache)
        
        cache
}
