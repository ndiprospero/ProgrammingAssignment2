## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #Store inverse as inv
  set <- function(y) { #Change matrix stored in makeCacheMatrix
    x <<- y #Substitute matrix x with input y entered in makeCacheMatrix
    inv <<- NULL #Restore value of inv to null
  }
  get <- function() x #Return matrix x stored with makeCacheMatrix
  setInverse <- function(Inverse) inv <<- Inverse #Cache inverse
  getInverse <- function() inv #Return cached invserse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) #Store four functions in makeCacheMatrix
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { #Input of cacheSolve is object where makeCacheMatrix is stored
  inv <- x$getInverse() #Return cached inverse
  if(!is.null(inv)) { #Verfiy value inv stored previously with makeCacheMatrix exists and isn't null
    message("getting cached data") #If inv exists, return message
    return(inv) #If inv exists, return value of inv
  }
  matr <- x$get() #Get matrix x stored with makeCacheMatrix
  inv <- solve(matr, ...) #Compute inverse of matrix x
  x$setInverse(inv) #Cache inverse
  inv # Return value of inv
}
