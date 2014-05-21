## function made by Carlos Martin-Mari as the second assignment for the coursera R Programming course

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL                     #null is set so we can differenciate when the inverse is already stored in cache
        set <- function(y){               #with that we can set a value for the matrix
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x                # inline function -get- to retrieve the matrix
        setinv <- function(inverse) inv_m <<- inverse  #inline function -setinv- storing inverse at inv_m
        getinv <- function() inv_m                     #inline function -getinv- retrieving inv_m
        list(set = set, get = get, setinv = setinv, getinv = getinv)   #all the different members of the function to get using
        # A <- makeCacheMatrix()
        # A$member()  
        # e.g: A$set(rbind(c(1,2),c(3,4)))
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()                              # retrieve the inverted matrix value from the makecachematrix environment
        if(!is.null(m)){                             # if it is already stored in the cache
                message("getting cached data")       # program just return m and we exit function printing out a message
                return(m)
        }
        data <- x$get()                #else... retrieves the matrix using get() from the makecache environment
        m <- solve(data, ...)          # and calculates the inverse
        x$setinv(m)                    # and sets it to store it in cache
        m                              # and returns it
}
