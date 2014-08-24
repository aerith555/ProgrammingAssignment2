#This function sets up helper functions to return the original values of the inverse and matrix to the cacheSolve function. It also caches the matrices so that they can be retrieved by the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
    rows <- nrow(x) #counts rows
    cols <- ncol(x) #counts columns
    inv <<- matrix(, rows, cols) #sets empty matrix with rows and columns
    set <- function(orig) {
        x <<- orig #sets a clone of the original matrix in cache
        inv <<- matrix(, rows, cols) #sets empty inverse in cache
    }
    
    get <- function() {
        x #function to return x from previous function
    }
    
    setinv <- function(z) {
        inv <<- z #prepares a function to set the inverse
    }
    
    getinv <- function() {
        inv #function to return inverse from previous function
    }
    
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

#This function retrieves the values of the matrices from the cache and solves for the value of the inverse.
cacheSolve <- function(x, ...) {
    inv <- x$getinv() #retrives value of the inverse
    if(!is.na(inv)) {
        return(inv) #if the inverse matrix is not empty, return the inverse
    }
    data <- x$get() #retrieves data from orinal matrix
    inv <- solve(data, ...) #solves for the inverse
    x$setinv(inv) #sets inverse
    inv ## Return a matrix that is the inverse of 'x'
}

#As I am new to programming (there's no R programming classes for high schoolers!), I relied heavily on the helper code to get through this assignment. If you have any suggestions for me on alternate ways I could have done this, please let me know!