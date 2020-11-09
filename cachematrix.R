## Put comments here that give an overall description of what your
## functions do

## the first function 'make cachematrix' creates a special 'matrix' object that can cache its inverse, which is really a list containing a function to set 
##the value of the matrix, get the value of the matrix, set the value of de inverse, get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<- function (z) {
                x<<-z
                inv<<-NULL
           }
        get<-function() x
        setsolve<- function(solve) inv<<-solve
        getsolve<- function() inv
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getsolve()
        if(!is.null(inv)) {
                message( "getting cached dat")
                return(inv)
                }
        data<-x$get()
        inv<- solve(data, ...)
        x$setsolve(inv)
        inv
}
