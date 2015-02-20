## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix - caches the inverse of a matrix
## get - returns the matrix
## set - sets env var x (i.e. the matrix)
## setInverse - sets env variable inv (i.e. the inverse)
## getInverse - returns contents of var inv
##
## cacheSolve - returns a matrix of the inverse of x
##            - if the inverse has previously been cached, use the cached value

## caches the inverse of a matrix in the inv var is the primary goal of this function

makeCacheMatrix <- function(x = matrix()) {
  
  # initial value for inv
  inv <- NULL
  set <- function(y) {
    # set x env var as the matrix
    x <<- y
    # if the matrix changes we need to set the inverse to null
    inv <<- NULL
  }
  # retuns the matrix
  get <- function() x
  # set the inv env var with the value of the passed var
  setInverse <- function(inverse) inv <<- inverse
  # return the inv env var
  getInverse <- function() inv
  list(set=set, 
       get=get, 
       setinverse=setInverse, 
       getinverse=getInverse)
}


## calculates the inverse of a matrix, if it has not previously been cached

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # call the getinverse function to get the value of the inverse
  inv <- x$getinverse()
  # it will be null if we have not calculated it
  if(!is.null(inv)) {
    # just a message to say we are using the cache
    message("Inverse retrieved from cache.")
    #return the inv var
    return(inv)
  }
  
  # if we get this far, we need to calculate the inverse
  
  message("Creating inverse for the first time.")
  # get the matrix value
  trix <- x$get()  
  # solve the matix value (inverse it)
  inv <- solve(trix)  
  # set the inverse in the cache
  x$setinverse(inv)  
  # return the inv
  return(inv)
  
}

#
# Just some testing
#
#
# > t$set(matrix(c(4,2,3,1),2,2))
# > t$get()
# [,1] [,2]
# [1,]    4    3
# [2,]    2    1
# > cacheSolve(t)
# Creating inverse for the first time.
# [,1] [,2]
# [1,] -0.5  1.5
# [2,]  1.0 -2.0
# > cacheSolve(t)
# Inverse retrieved from cache.
# [,1] [,2]
# [1,] -0.5  1.5
# [2,]  1.0 -2.0
# 
# Change the matrix
# > t$set(matrix(c(5,3,2,8),2,2))
# > t$get()
# [,1] [,2]
# [1,]    5    2
# [2,]    3    8
# > cacheSolve(t)
# Creating inverse for the first time.
# [,1]        [,2]
# [1,]  0.23529412 -0.05882353
# [2,] -0.08823529  0.14705882
# > cacheSolve(t)
# Inverse retrieved from cache.
# [,1]        [,2]
# [1,]  0.23529412 -0.05882353
# [2,] -0.08823529  0.14705882
#
#

