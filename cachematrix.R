## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## -makeCacheMatrix- 
## 1) sets the value of a matrix to -x- and creates an empty value, -m-, that will be later populated with the called function -solve(x)-
## 2) gets the value of the matrix
## 3) sets the inverse of the value of the matrix using the -solve- function
## 4) gets the value of -m-, the inverse of the matrix, for later recall

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvs <- function(solve) m <<- solve
  getinvs <- function() m
  list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)
}


## Write a short comment describing this function
## -cacheSolve-
## 1) calls -m- from $getinvs
## 2) checks if -m- is a populated object and,
##    if so, it returns the already calculated matrix inverse
##    if not, it passes to the next set of functions to calculate the matrix inverse and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvs()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvs(m)
  m
}
