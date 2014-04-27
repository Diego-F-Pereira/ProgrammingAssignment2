# Function    : makeCacheMatrix
# Purpose     : Creates a special "matrix" object that can cache its inverse
# Arguments   : x = A numeric squared matrix
# Usage       : C <- makeCacheMatrix(matrix(c(1,2,4,2,1,1,3,1,2), nrow=3))

  makeCacheMatrix <- function( x = numeric() ) {
    i <- NULL
    set <- function( y ) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setsolve <- function( solve ) i <<- solve
    getsolve <- function() i
    list( set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve )
  }

# Function    : cacheSolve
# Purpose     : Computes the inverse of a squared matrix cached by the
#               function makeCacheMatrix()
# Description : Uses the solve() function for calculating the inverse matrix
# Arguments   : x = An object created by the makeCacheMatrix() function
# Usage       : cacheSolve(C)
  
  cacheSolve <- function( x, ... ) {
    i <- x$getsolve()
    if( !is.null( i ) ) {
      message( "getting cached data" )
      return( i )
    }
    data <- x$get()
    i <- solve( data, ... )
    x$setsolve( i )
    i
  }
