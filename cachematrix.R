## Author: nhatmn
## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The functions below are to cache the inverse of a matrix.
## To use these functions, I assume that the matrix supplied is always invertible.
## So I won't check if the input matrix is invertible or not.

## Author: nhatmn
## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
## Arguments:
## * mtx: an invertible matrix
## Usage (For example):
## > matrix <- matrix(c(1,2,3,0,1,4,5,6,0),3,3)
## > cacheMatrix <- makeCacheMatrix(matrix)

makeCacheMatrix <- function(mtx = matrix()) {
    iMatrix <- NULL
    set <- function(y) {
        mtx <<- y
        iMatrix <<- NULL
    }
    get <- function() return(mtx)
    setiMatrix <- function(inv) iMatrix <<- inv
    getiMatrix <- function() return(iMatrix)
    list(set = set, get = get,
         setiMatrix = setiMatrix,
         getiMatrix = getiMatrix)
    
}

## Author: nhatmn
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
## Arguments:
## * mtx: an invertible matrix
## * ...: The other arguments of solve function.
## Usage (For example):
## > cacheSolve(cacheMatrix)

cacheSolve <- function(mtx, ...) {
    iMatrix <- mtx$getiMatrix()
    if(!is.null(iMatrix)) {
        message("getting cached data")
        return(iMatrix)
    }
    data <- mtx$get()
    iMatrix <- solve(data, ...)
    mtx$setiMatrix(iMatrix)
    iMatrix
}
