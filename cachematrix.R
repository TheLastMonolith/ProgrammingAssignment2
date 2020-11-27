# This R script contains two functions: makeCacheMatrix() and cacheSolve()
# The first function, makeCacheMatrix(), creates an R object that stores a matrix
# and it's resulting inverse matrix (to be computed by the 2nd function).
# The 2nd function, cacheSolve(), performs the inverse of the matrix calculation
# using the solve() function, and stores (caches) it back to the makeCacheMatrix() 
# object environment.


## makeCacheMatrix() function
#
## This function contains two objects x and inv_m, and four functions:
##      set = sets the value of the objects x and inv_m.
##      get = returns the matrix object (x)
##      setinverse = sets the value of the inverse of the matrix (inv_m)
##      getinverse = returns the value of the inverse of the matrix.
## The first part of this function initializes the value of x and inv_m.
## The second part creates the "setters" and "getters" functions that will set
## the data values within an object (e.g. x, inv_m) and retrieves the value of 
## an object.
## Lastly, this function also builds a list that assigns each nested functions 
## to its elements, which can be later on be returned on the parent environment
## with the help of lexical scoping.


makeCacheMatrix <- function(x = matrix()) {
            inv_m <- NULL
            set <- function(y){
                    x <<- y
                    inv_m <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) inv_m <<- inverse
            getinverse <- function() inv_m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## cacheSolve function
#
## This function takes the object created by the makeCacheMatrix() function
## and returns the cached inverse of the matrix if available. 
## It performs the inverse calculation, thru solve(),if the there is no data cached
## on the inv_m object of the makeCacheMatrix() environment and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv_m <- x$getinverse()
            if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
            }
            data <- x$get()
            inv_m <- solve(data, ...)
            x$setinverse(inv_m)
            inv_m
}

