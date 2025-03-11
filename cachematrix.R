## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL

        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        get <- function() {
                m
        }

        set_inverse <- function (inverse) {
                i <<- inverse
        }

        get_inverse <- function() {
                i
        }

        list( set = set, get = get, 
             set-inverse = set_inverse
             get_inverse = get_inverse)
        }


## Computing the inverse of the special matrix returned by the other function "makeCacheMatrix" above. If the inverse has 
## alreday ben calculated and if the matrix hasn't changed. Then the this function, "cacheSolve" should retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()

        if ( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()

        m <- solve(data) %*% data

        x$set_inverse(m)

        m
}
