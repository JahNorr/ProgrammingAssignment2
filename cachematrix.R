## ######################################################################
##  These functions form a pair where one serves as a cache
##   for storing some data and the results of a lengthy calculation based on that data
##   and the other function uses the first function to determine whether or not
##   the calculation has been done already (matinv != NULL), and, 
##   if not (matinv == NULL),
##   executes the calculation and stores the result
## 
## makeCacheMatrix allows the storage of a matrix, 
## it ititializes (sets to NULL) the cached variable (matinv) upon call to
## the function itself or the set() internal function
## there are internal functions to retrieve the stored data upon which the calculation
## will be performed and functions to set and get the cached variable

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize cached variable of interest
    matinv<-NULL
    
    # store the data to be used in the lengthy calculation
    #  and initialize the cached variable
    set<-function(y) {
        x<<-y
        matinv<<-NULL
    }
    
    # get the data to be used in the lengthy calculation
    get<-function() {
        x
    }
    
    # set the variable to be cached
    setinverse<-function(minv){
        matinv<<-minv
    }
    
    # get the cached variable
    getinverse<-function(){
        matinv
    }

    #return a list of the internal functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## returns a matrix that is the inverse of the matrix that has been
## stored in 'x' ... the computed inverse is also stored in 'x' if it has been
##  computed
## 

cacheSolve <- function(x, ...) {
    ## get the cached variable to see if it has been set (!=NULL)

    mat <- x$getinverse()
    if(!is.null(mat)) {
        ## it has been set (!=NULL) so just retrieve the cached value and return
        message("getting cached data")
        return(mat)
    }
    ## the cached variable is NULL so get the datan(matrix), use the data to compute the
    ##  inverse, cache the inverse, and return the inverse
    
    data <- x$get()
    mat <- solve(data)
    x$setinverse(mat)
    mat
}


