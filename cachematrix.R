## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function "makeCacheMatrix" gets a matrix as an input, sets the value of the matrix,		
## gets the value of said matrix, sets the inverse matrix, then gets said inverse matrix 
## - Thus 'caching' its own inverse!

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
     x <<- y
     inv <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
 }


## Write a short comment describing this function

## The function "cacheSolve" takes the output from the above function, "makeCacheMatrix", as an input and
## evaluates the inverse matrix from "makeCacheMatrix"		
## i)  In the event that the inverse matrix is empty, it retrieves the original matrix data and	
##     sets the invertible matrix by using the solve function.		
## ii) In the event that the inverse matrix returns a value, the program displays the message "Retrieving cached data", and
##     gets the 'cached' inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
     message("retrieving cached data")
     return(inv)
   }
   data <- x$get()
   inv <- solve(data, ...)
   x$setinverse(inv)
   inv
 }
