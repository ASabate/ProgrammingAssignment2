## Put comments here that give an overall description of what your
## functions do
# R Programing Coursera
# Assignment 2
# Date: 25/10/2015
## Write a short comment describing this function
# function: makeCacheMatrix 
# input: invertible matrix
# Description: creates a list containing a function to
# 1.set the matrix
# 2.get the matrix
# 3.set the inverse matrix
# 4.get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinv <- function(invmat) inversematrix <<- invmat
  getinv <- function() inversematrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# function: cacheSolve
# input: list of arguments from the prev funct
# output: inverse matrix
# Description: 
# Calculates the inverse of the matrix, taking the 
# arguments calculated at the above function. It checks 
# first if it has already been calculated. If so, it gets 
# the inverse matrix from the chache and skips the 
# computation. Otherwise, it calculates the inverse matrix, 
# and sets the value in the cache via setinv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversematrix <- x$getinv()
    # if inversematrix has already been calculated
    if(!is.null(inversematrix)) 
    {
      message("getting cached data")
      # gets it from cache and skips computation
      return(inversematrix)
    }
    # otherwise, it calculates the inverse
    mat.data <- x$get()
    inversematrix <- solve(mat.data, ...)
    # sets the value of the inverse matrix in the cache
    x$setinv(inversematrix)
    inversematrix       
}
