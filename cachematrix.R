## [Put comments here that describe what your functions do]

## Programming Assignment 2 - Lexical Scoping
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache its inverse
    ## Resetting and preparing
    inversematrix<-NULL
    setmatrix<-function(matrix){
      x<<-matrix
      inversematrix<<-NULL
    }
    ## Get data
    getmatrix<-function() x
    setinversematrix<-function(solve) inversematrix<<- solve
    getinversematrix<-function() inversematrix
    list(setmatrix=setmatrix, getmatrix=getmatrix,
         setinversematrix=setinversematrix,
         getinversematrix=getinversematrix)
}


cacheSolve <- function(x, ...) {
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
    inversematrix<-x$getinversematrix()
    if(!is.null(inversematrix)){
      message("getting data from cache")
      return(inversematrix)
    }
    refreshdata<-x$getmatrix()
    inversematrix<-solve(refreshdata, ...)
    x$setinversematrix(inversematrix)
    inversematrix
}
