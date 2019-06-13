##This script file contains two functions 
##1. makeCacheMatrix which are used to cache a non-singular matrix data. 
##2. cacheSolve to calculate the inverse of the matrix, and to store the solution
## in the cache, only if it has not being calculated. 
##If the inverse has been already calculated, it loads the cached inverted matrix.

## makeCacheMatrix function creates a vector to store and perform basic functions
## of set a matrix, retrieve matrix, set inverse, retrieve onverse.
## makeCacheMatrix() need to be run with a square, non singular martix
##as its argument and assign the return value to a variable
## usage eg s1<-makeCacheMatrix(v1)
## or indicidyal member functions eg s1$setmatrix(v1) where vi is a matrix

makeCacheMatrix <- function(x = matrix()) {
    myinverse <- NULL
    setmatrix <- function(y){
                    x <<- y
                    myinverse <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(mysolve) myinverse <<- mysolve
    getinverse <- function() myinverse
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse=getinverse)
}


## This function is used to 
##1. Retrieve, the inverse if it has been already cached
##2. calculate, store the inverse in cache and retrieve the inverse, if it has 
## not already being calculated
## usage eg: cacheSolve(s1)  - will not give the message in first run and will 
## give message "getting cached inverse matrix" in consequent runs
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        myinverse <- x$getinverse()
        if(!is.null(myinverse)){
            message("getting cached inverse matrix")
            return(myinverse)
        }else{                              ## Can be written without "else", as example
            mymatrix <- x$getmatrix()
            myinverse <- solve(mymatrix,...)
            x$setinverse(myinverse)
            return(myinverse)
        }
        
}
