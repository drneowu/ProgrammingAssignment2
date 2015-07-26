## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes as argument a matrix object, and creates a 
# closure that contains the matrix data, a matrix object that can be used # to store the cache of the inverse of x, and several getter and setter 
# functions, in order to get and set the data , and get and set the invers e.  

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # the cached inverse of the matrix
    set <- function (y) { # init a new matrix, set cache to NULL
        x <<- y
        inv <<- NULL
    }
    get <- function() x # return the matrix
    setinv <- function(sol) inv  <<- sol
    getinv <- function()  inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
    # return a list of four functions

}


## this function first determines whether the inverse of matrix x 
# ( the underlyding data is stored in x) has been calculated. 
# If so, it simply retrieves the inverse, otherwsie 
# it calculates the inverse, and cache it inside x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
    inv <- x$getinv()
    if (!is.null(inv)) {      #if it is cached
        return(inv)           # return a cached inverse
    }
    data <- x$get()
    inv <- solve(data)        #calculate a new inverse
    x$setinv(inv)             # and cache it
    inv
}
