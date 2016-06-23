## These two fucntions will be used to calculate the inverse of a given matrix
## and cache the results for faster retrievel later.

## This function takes in a matrix and sets and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setmatrix <- function(inverse) m <<- inverse
    getmatrix <- function() m
    list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function will check to see if data is already cached; 
## if it has been cached, it will retrieved the previous cached value; 
## else it will calculate the inverse and cach the data

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}  