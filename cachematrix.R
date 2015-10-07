# This function will create a matrix and calculates inverse of the matrix as well 
# cache the value of matrix inversion
makeCacheMatrix <- function(x = matrix()) {
      mat_inverse <- NULL
      set <- function(y) {
      x <<- y
      mat_inverse <<- NULL
      }
      get <- function() x
      set_inverse <- function(inverse) mat_inverse <<- inverse
      get_inverse <- function() mat_inverse
      list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


# The following function returns the inverse of the matrix. It first checks if invert
#is available in cache if so retrieves that else calculates inverse


# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
      mat_inverse <- x$getinverse()
      if(!is.null(inv)) {
          message("getting cached data.")
          return(mat_inverse)
      }
      data <- x$get()
      mat_inverse <- solve(data)
      x$set_inverse(mat_inverse)
      mat_inverse
}