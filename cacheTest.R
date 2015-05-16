source("./cachematrix.R")


mm<-matrix(c(1,0,0,0,1,0,1,1,1), nrow = 3, ncol = 3)

mmcache<-makeCacheMatrix(mm)
cacheSolve(mmcache)

cacheSolve(mmcache)


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

