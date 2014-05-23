## Title           :    Programming Assignment 2 - Cache Inverse of a Matrix
## Author          :    Jagan Kondapalli
## Last Updated    :    05/23/2014
## Description     :    This Script calculates the Inverse of a matrix and stores it in cache. 
##                      Should the user run the code for calculating the Inverser of the same matrix, 
##                      Code retreives it from cache instead of re-calculating the Inverse 

## This function provides options to the user whether to set, get a matrix and whether to set or get Inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  # if an object is called without a method
  Inv <- NULL                       # initializing Inv as NULL
  set <- function(y)                # Set function to set matrix to a variable
  {
    x <<- y                        # when a$set(z) is called z is set to z matrix
    Inv <<- NULL                   # re-initializing Inv as NULL
  }
  get <- function()                 # Get function to get the already set matrix
  {
    x                                # Just spits out x to the console
  }
  setInverse <- function(Inverse)   # Calculate Inverse
  {
    Inv <<- Inverse
  }
  getInverse <- function()          # Get Inverse
  {
    Inv
  }
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse) # list that is put out to CacheSolve function
}



## This function calculates the Inverse of the matrix

cacheSolve <- function(x, ...) 
{
  Inv <- x$getInverse()
  if(!is.null(Inv))           # if the a$getInverse() is not equal to null then spit out the value of Inv else continue calculating Inverse
  {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  #print(data)
  Inv <- solve(data, ...)
  x$setInverse(Inv)
  Inv
}
