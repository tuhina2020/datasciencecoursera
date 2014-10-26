## Making a vector of functions to set,get the matrix and its inverse

makeCacheMatrix <- function(x=matrix()) {
     i <- NULL
     set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) i <<- inverse
        get_inverse <- function() i
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

}


## Caching the inverse matrix values if they have been already calculated

cacheSolve <- function(x=matrix(),...) {

        ## Return a matrix that is the inverse of 'x'
        l <- makeCacheMatrix(x)
        i <- l$get_inverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data1 <- l$get()
        i <- solve(data1)
        l$set_inverse(i)
        i <- l$get_inverse()
        i
}
