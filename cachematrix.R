## Function makeCacheMatrix should return a list with functions that set the matrix,
## get the matrix, set the inverse, and get the inverse, where x is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
      
      inv = NULL
      set = function(y) {
            
            x <<- y
            inv <<- NULL
      }
      
      get = function() x
      setinv = function(inverse) inv <<- inverse 
      getinv = function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Function cacheSolve takes the output x of makeCacheMatrix and returns the inverse of
## the starting matrix

cacheSolve <- function(x, ...) {
      
      inv = x$getinv()
      
      # This loops detects if the matrix is already inverted. If it is, it simply prints it from
      # the cache
      if (!is.null(inv)){
            
            return(inv)
      }
      
      # This is the else condition, it calculates the inverse matrix
      mat.data = x$get()
      inv = solve(mat.data, ...)
      
      # This line caches the value of the inverse
      x$setinv(inv)
      
      return(inv)
}