## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix(): Creates a matrix object that can cache its inverse
#cacheSolve(): computes the inverse of the matrix returned by makeCacheMatrix(). If the inverse has already been calculated 
#it retrieves the value from the cache

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#@x: a square invertible matrix
  ## return: list with the below functions an input to cacheSolve
  # 1. set the matrix
  # 2. get the matrix
  # 3. set the inverse
  # 4. get the inverse
  ## the list is used as the input to cacheSolve()
  
  inv<-NULL
  set<-function(y){
    # assigning a value to an object 
    x<<-y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## @x: makeCacheMatrix() output 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        
        #if the inverse has already been calculated
        if (!is.null(inv)) {
          # get it from the cache and skips the computation 
          message("getting cached data")
          return (inv)
        }
          
# otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
# sets the value of the inverse in the cache through the setinv function
        x$setinv(inv)
        return (inv)
}
