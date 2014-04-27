## Functions makeCacheMatrix and cacheSolve compute matrix inversion, 
## but first check whether it was already computed earlier, and if so, 
## don't compute it directly, but take it from the cache. This allows you
## to reduce the time of computation, when matrix is rather big, 
## especially when matrix inversion is computed repeatedly.

## To use this function, first create a special object using 
## YourCacheMatrix<-makeCacheMatrix(YourMatrix), and then call 
## cacheSolve(YourCacheMatrix) as many times, as you need.


## Function makeCacheMatrix(x) creates a special object of the matrix x, 
## which contains methods:
## set(y) - set matrix value to y
## get() - returns the value of matrix
## setinv(Inverse) - set cached matrix inversion value to Inverse
## getinv() - get the inversion value

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y) {
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) inv<<-inverse
    getinv<-function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}

## CacheSolve function takes the object, created by makeCacheSolve function 
## of matrix x, as an input, and returnes the inverse of x. However, first 
## it checks, if this inversion has already been computed, and if so, returnes 
## the cached value of this matrix inversion with the message "getting cached 
## data".

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setinv(inv)
    inv
}

