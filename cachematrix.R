## These two functions create and maintain a novel matrix (effectively a list) that
## keeps a matrix and its inverse. The novel matrix's advantage is that its inverse
## is not recalculated every time it is asked by the user 'x$getinv()'. It is only 
## calculated if a new matrix is entered 'x$set(<some_matrix>)'. This is achieved by
## using R's caching capability of objects in alternative environments to those 
## visible within a function


## This function creates a quadruple of functions that encapsulate the value of a
## matrix and its inverse both held in its internal memory space

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # initialise inverse matrix to null
  
  # Define 'set' function that...
  set <- function(y) {
    x <<- y # stores new matrix value in cache
    inv <<- NULL # nullifies previous inverse of matrix (to force recalculation)
  }
  
  # Returns matrix
  get <- function() x
  
  # Sets the inverse of the matrix having been calculated elsewhere
  setinv <- function(invs) inv <<- invs
  
  # Returns the inverse of a matrix
  getinv <- function() inv
  
  # Constructs the list with the 4 previously defined functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function accepts one or more matrices as input and calculates their inverse
## If the inverse of these has already been calculated and was stored in cache it just
## returns it, thus saving computational time

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  myinv <- x$getinv() # call getinv() on x to see if the inverse has been pre-calculated
  
  if(!is.null(myinv)) { # if the inverse was calculated before...
    message("getting cached data") # notify the user
    return(myinv) # just return the inverse
  }
  #... otherwise
  data <- x$get() # get the matrix
  myinv <- solve(data, ...) # calculate its inverse
  x$setinv(myinv) # set its inverse in its internal cache
  myinv # show/return the result to the user
}
