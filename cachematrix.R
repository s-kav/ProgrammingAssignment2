## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## ok, let's start :-)

makeCacheMatrix <- function(x = matrix()) {
library(matlib)
## we use special package "matlib" for a calculation of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
## initialize object "x" as a function argument - matrix
        MM <- NULL
## initialize as object "MM" within the  environment of function "makeCacheMatrix"
        set <- function(y = matrix()) {
## initialize object "y" as a function argument - matrix, but not directly
                x <<- y
## to assign the variable "y" to an object in the parent environment, in our case it's the object "x" (function argument)
                MM <<- NULL
        }
        get <- function() x
## variable "x" retrieves it from the parent environment
        setInvers <- function(inv) MM <<- inv
## variable "MM" defines for the inverse matrix via function "inv"
        getInvers <- function() MM
## to assign each of these functions (4 functions) as an element within "list" and to return to the parent environment
        list(set = set, get = get,
             setInvers = setInvers,
             getInvers = getInvers)
}

cacheSolve <- function(makeCacheMatrix.object, ...) {
        MM.local <- makeCacheMatrix.object$getInvers()
## to attempt to retrieve an inverse matrix from the object passed in as the argument to this function
        if (!is.null(MM.local)) {
                message("getting cached data")
                return(MM.local)
        }
## checking the result is NULL
        data <- makeCacheMatrix.object$get()
        MM.local.calculated <- inv(data, ...)
## to calculate an inverse matrix, uses "setInvers" function as the input object
        makeCacheMatrix.object$setInvers(MM.local.calculated)
        MM.local.calculated # return the value of inverse matrix
}
