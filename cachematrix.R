## The following two functions create a square invertible matrix
## and make the inverse of the matrix available in the cache environment

## This first function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        get <- function() x
        setInversa <- function(inversa) inversa <<- inversa
        getInversa <- function() inversa
        list(set = set,
             get = get,
             setInversa = setInversa,
             getInversa = getInversa)
}


## This second function computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversa <- x$getInversa()
        if ( ! is.null(inversa)) {
                message("getting cached data")
                return(inversa)
        }
        dados <- x$get()
        inversa <- solve(dados)
        x$setInversa(inversa)
        inversa
}
