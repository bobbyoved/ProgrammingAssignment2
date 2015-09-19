##Description: This function creates a 'special' matrix object
##            This object can cache it's inverse
##            The user enters a vector and if the vector length
##            divides by 2, then it can be created as a square matrix

makeCacheMatrix <- function(x = matrix()) {
  #set to Null initially
  m<-NULL
  set<- function(y){
    
    #check to see if the input vector divides by 2 evenly
    if(length(x)%%2==0){
      #make the matrix from the vector the user inputs
      y<-matrix(x,length(x)/2, length(x)/2) 
      x <<-y
      m <<-NULL
    }
    #print an error stating vector will not produce a square matrix
    else{
      message("Error: input vector will not produce a square matrix!!")
      
    }
  }
  #create the get function
  get<-function() x
  
  #create the set inverse function for setting the matrix inverse
  setInverse <-function(inverse) m <<- inverse
  
  #create the get inverse function for getting the matrix inverse
  getInverse <- function() m
  
  #create the list that stores the list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

 ## Function:cacheSolve
## Description: This function computes the special 'matrix'
##              returned by the function 'makeCacheMatrix'.
##              If the inverse is already calculated and the
##              matrix has not changed, then this function should
##              retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #do this by subsetting the getInverse() function
  m<- x$getInverse()
  if(!is.null(m)){
    message("getting the cached inverse matrix")
    return(m)
  }
  #subset the get() function
  data <- x$get()
  
  #compute the inverse of m using the solve function
  m<-solve(data,...)
  
  #subset the setInverse function
  x$setInverse(m)
  m
}
