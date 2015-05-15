library(MASS)

## ###############################################################
##
## 
## 
## makeCacheMatrix allows the storage of a matrix, 
## set its inverse when the setinverse function is called and stores it
## calls to set and calls to the main function reset the stored value to NULL

makeCacheMatrix <- function(x = matrix) {
    matinv<-NULL
    
    set<-function(y) {
        x<<-y
        matinv<<-NULL
    }
    
    get<-function() {
        x
    }
    
    setinverse<-function(minv){
        matinv<<-minv
    }
    
    getinverse<-function(){
        matinv
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mat <- x$getinverse()
    if(!is.null(mat)) {
        message("getting cached data")
        return(mat)
    }
    data <- x$get()
    mat <- ginv(data, ...)
    x$setinverse(mat)
    mat
}

mm<-matrix(c(1,2,3, 11,12,13, -3,-2,-1), nrow = 3, ncol = 3)

mmcache<-makeCacheMatrix(mm)
cacheSolve(mmcache)


