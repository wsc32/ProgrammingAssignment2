## The <<- operator is used to assign a value to an object in an 
## environment that is different from the current environment.

## The two functions below are used to create a special object that
## stores a matrix and cache its inverse. 

## The first function, makeCacheMatrix creates a special "matrix",
## which is a matrix containing a function to
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse of the matrix
## 4. get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) im <<- solve
        getsolve <- function() im
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This second function, cacheSolve, calculates the inverse of the 
## special matrix created with the above function.
## But it firsts checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the 
## matrix and sets the values of the inverse in the cache via the 
## setsolve function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getsolve()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setsolve(im)
        im
}