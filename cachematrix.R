## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function creates a square invertable matrix with the following functions and caches its values:
## 1) set(): sets the values inside the matrix
## 2) get(): gets the value of the matrix
## 3) set_inverse(): sets the inverse of the matrix
## 4) get_inverse(): gets the inverse of the matrix
## 5) list(): used for input in cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse <- NULL
    set <- function(y) {
        # '<<-' assigns a value to an object in the environment
        x <<- y
        matrix_inverse <<- NULL
    }
    
    # functions that are returned for cacheSolve()
    get <- function() x
    set_inverse <- function(inverse) matrix_inverse <<- inverse
    get_inverse <- function() matrix_inverse
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

## The function checks to see if the matrix has been created from makeCacheMatrix(). 
## If so, the inverse of the matrix that was created is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # x is the output of makeCacheMatrix()
    
    inverse <- x$get_inverse()
    
    # If the inverse has been calculated 
    if (!is.null(inverse)) {
        # The message appears and the value of inverse is returned from the cache
        message("getting cached data")
        inverse
    }
    
    # else the inverse is calculated using solve(), 
    # which requires a square matrix as the first argument
    matrix_data <- x$get()
    inverse <- solve(matrix_data, ...)
    
    # set the inverse in the cache from makeCacheMatrix() and return the inverse
    # subsequent calls should use the cache if the result has been stored
    x$set_inverse(inverse)
    inverse
}
