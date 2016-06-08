## The following functions work in pair to calculate the inverse of a matrix and cache it for later retrieval.

## This function makes a special Matrix which is a list of functions containing the following :
# set(x) : set the value of the matrix
# get() : get the value of the matrix
# setinverse(i) : set the inverse of the matrix
# getinverse() : get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function uses the list of function defined above: 
# if the inverse is being calculated for the first time, saves it to the cache, and return the value
# else retrieve the already saved value from the cache.

cacheSolve <- function(x, ...) {
        i<- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat<-x$get()
        I <- diag(nrow(mat))
        i<- solve(mat,I)
        x$setinverse(i)
        i
}
