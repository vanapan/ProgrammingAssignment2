

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.


## Example of how we can use these functions to compute the inverse of a 2x2 matrix.
## > x = rbind(c(1, -2), c(3, 4))
## > m = makeCacheMatrix(x)
## In the first attempt, the inverse is not in the cache, and so it is computed
## > cacheSolve(m)
## [,1] [,2]
## [1,]  0.4  0.2
## [2,] -0.3  0.1
## In the second attempt, the inverse is already in the cache, so instead of compute it
## again, it just retrieves the result from the cache
## > cacheSolve(m)
## getting cached data
## [,1] [,2]
## [1,]  0.4  0.2
## [2,] -0.3  0.1



## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function( m = matrix() ) {
       
       
       ## Initialization of the variable that holds the cached value
       ## NULL if nothing is cached 
       
       i <- NULL
       
       ## Set the matrix
       set <- function( new_matrix ) {
              
              ## Assign the input matrix new_matrix to the variable m
              m <<- new_matrix

              
              ## Since a new matrix has been assigned, re-initialize the
              ## cached value
              i <<- NULL
       }
       
       
       ## Get the matrix
       get <- function() {
              ## Return the stored matrix
              m
       }
       
       ## Set the inverse of the matrix
       setInverse <- function(inverse) {
              
              ## Set the cached value i equal to the inverse 
              ## of the matrix m
              i <<- inverse
       }
       
       ## Get the inverse of the matrix
       getInverse <- function() {
              
              ## Return the cached inverse of m
              i
       }
       
       ## Return a list of the functions
       list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}





## The following function calculates the inverse of a "special" matrix created with 
## makeCacheMatrix. It first checks if the inverse has already been computed.
## If so, it gets the result from the cache and skips the computation.
## If not, it computes the inverse and sets the value of the inverse in the cache via
## the setInverse function.
## Note: the function assumes that every given matrix is intertible.

cacheSolve <- function(x, ...) {

       
       ## Return a matrix that is the inverse of 'x',
       ## by getting the cached value
       m <- x$getInverse()
       
       ## If a cached value exists return it
       
       if( !is.null(m) ) {
              message("getting cached data")
              return(m)
       }
       
       ## If there is no cached value, get the matrix, 
       ## caclulate the inverse and store it in the cache
       
       ## Get the matrix from our object
       data <- x$get()
       
       ## Calculate the inverse using matrix multiplication
       m <- solve(data) 
       
       ## Set the inverse to the object
       x$setInverse(m)
       
       ## Return the inverse matrix
       m
}
