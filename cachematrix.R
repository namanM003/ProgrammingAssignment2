## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## mackeCacheMatrix this function takes a matrix and creates a type of list 
##1. set the value of matrix
##2. get the value of matrix
##3. set the inverse of matrix
##4. get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## Write a short comment describing this function
##cachesolve function calculates the mean of the "matrix" created by makeCacheMatrix.
##it is first checking if the inverse for the given matrix has been calculated
##If it has been calculated earlier then it just simply returns it else it calculates it and set 
## inverse in the cache by setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("Getting cached inverse of matrix")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ... )
        x$setinverse(inverse)
        inverse
}
