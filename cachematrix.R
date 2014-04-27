## A couple of first class functions (similar to the concept of object in Perl/Python
## are built to allow for a "special" type of matrix object. What is special about
## this object is that by using makeCacheMatrix, it can store, apart from the matrix,
## the inverse matrix, and the functions to read and write the inverse. The inverse
## can be calculated using the functon cacheSolve

## This function simply stores a matrix as a "special matrix" object. 
## The  "special matrix" object, apart from holding the matrix, also holds
## it's inverse (initially NULL), and can also read and write the matrix itself
## and its inverse using the get, set, set_inverse and get_inverse functions

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL        
    }
    
    get <- function() x
    set_inverse <- function(matrix_inverse) inverse <<- matrix_inverse
    get_inverse <- function() inverse
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function is calulating the inverse of a "special matrix" object. It expects 
## a "special matrix" and if the inverse has not been calculated before, it goes
## to calculate it and stores the inverse in the "special matrix". Otherwise, it 
## returns the already calculated inverse of the "special matrix". 
cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if (!is.null(inverse)) {
        message("getting cached inverse of matrix")
        return(inverse)        
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$set_inverse(inverse)
    ## Return a matrix that is the inverse of 'x'
    inverse
}
