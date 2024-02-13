## This program implement cached inverse matrix calculation
##   It is accomplish by emulating in some way POO capabilities,
##   using functions to update and get values across environments as object properties 
  

makeCacheMatrix <- function(x = matrix()) {
       if (nrow(x) != ncol(x)){
         stop("Square matrix it's expected")
       }

       if(det(x) == 0 ){
         stop("Determinant non zero it's expected")
       }

      result <- NULL
      set <- function(y) {
                x <<- y
                result <<- NULL
      }
      get <- function() x
      setinverse <- function(inverted) result <<- inverted
      getinverse <- function() result

      list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Verify if the object already has the inverted matrix stored
##   Positive case: the stored inverted matrix is return
##   Negative case: `solve(x)` is called and result, future calls will use cached data

cacheSolve <- function(x, ...) {
       i <- x$getinverse()
       if(!is.null(i)) {
                message("cached..")
                return(i)
       }
       data <- x$get()
       i <- solve(data,...)
       x$setinverse(i)
       ## Return a matrix that is the inverse of 'x'
       i
}
