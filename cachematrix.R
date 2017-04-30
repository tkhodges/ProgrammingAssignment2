#Programming Assignment#2 - Caching the Inverse of a Matrix
##For the assignment, a pair of functions was written to cache the inverse of a matrix

##makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 
##The function builds a set of functions and returns the functions within a list
makeCacheMatrix <- function(x = matrix()) {
          oMat <- NULL
          set <- function(y){
                        x <<- y
                        oMat <<- NULL
          }
          get <- function() x
          set_matrix <- function(inverse) oMat <<- inverse
          get_inverse <- function() oMat
          list(set = set, get = get,
               set_matrix = set_matrix,
               get_inverse = get_inverse)
          
            }
  

##cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
          oMat <- x$get_inverse()
          if(!is.null(oMat)) {
          message("getting cached data")
          return(oMat)
          }
          my_matrix <- x$get()
          oMat <- solve(my_matrix, ...)
          x$set_matrix(oMat)
          oMat
}






