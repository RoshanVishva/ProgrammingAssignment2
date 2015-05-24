## This functions cache the inverse of a matrix. And use the computed inverse 
## matrix whenever it use next.


## This function creates a special matrix object that can cache its inverse. 
##The special Matrix is created is a list consist of set functions as list elements
## to serve different purposes. 

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  # setting the matrix to 1st argument of set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Return the value in symble x
  get <- function() x
  
  # argument sent to the function will be assigned to m
  setinverse <- function(invs) m <<- invs
  
  # Function is tp retrun value in m ,
  getinverse <- function() m
  
  # Making the list with functions
  list(set = set , get=get ,setinverse=setinverse ,getinverse=getinverse)

}

##############################################################################
## This function computes the inverse (or returned cached inverse matrix if already 
## exist) of the special matrix objects created by using the function makeCacheMatrix

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'

 #fetching the value of m global variable in the  argument(x) using  
 #getinverse
 
  m <- x$getinverse()

  # If  inverse already been computed for passed 
  #argument (no value of in it)

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if no inverse been computed for passed argument x (specit matrix)
  #Fetching value inside 1st argument(specit matrix)  using get function

  data <- x$get()

  #computing inverse matrix

  m <- solve(data, ...)

  #Assign computed inverse matrix to m in side 1st argument(special matrix)

  x$setinverse(m)

  # retrun the computed inverse matrix
  m
}
