## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix is used to set and get the matrix 
## and its inverse in cache

makeCacheMatrix <- function (x = matrix()) {
        
        minv <- NULL
        set <- function (y = matrix()) {
                x <<- y
                minv <<- NULL
        }
        
        get <- function() x
        setmatrix <- function(solve) minv <<- solve 
        getmatrix <- function() minv
        list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
        
}


## This function calculates the inverse of a matrix or gets it from the
## cache

cacheSolve <- function (x, ...) {
        ## Return a matrix that is the inverse of 'x'

        minv <- x$getmatrix
        
        if(!is.null(minv)){
                message("getting matrix from cache")
                return(minv)
        }
        
        matrix <- x$get
        m <-solve(matrix, ...)
        x$setmatrix(m)
        m
}
