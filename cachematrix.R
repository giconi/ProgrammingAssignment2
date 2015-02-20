## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  cmatrix <<- x
  inverse <<- solve(x)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  if(exists("cmatrix")){
        
    if(!identical(x,cmatrix)){
      
      message("Matrix has changed, removing inverse.")
      if(exists("inverse")){
        rm(list = ls(envir=globalenv())[
          grep("inverse", ls(envir=globalenv()))], envir = globalenv())
      }
    }
    
  }
  
  if(!exists("inverse")){
    message("Calculating inverse")
    makeCacheMatrix(x)
  }else{
    message("using cache")   
  }
  
  return(inverse)
  
}
