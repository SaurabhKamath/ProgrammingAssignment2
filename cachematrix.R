##############################################################################################################################
#############################################  Caching the Inverse of a Matrix  ##############################################
##############################################################################################################################
## Matrix inversion is usually a costly computation and their 
## may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.

## In this script you can find two functions "makeCacheMatrix" 
## and "cacheSolve" used to cache inverse of a given input 
## matrix.

## Below you can find the script definition and description:

## Function "makeCacheMatrix" is used to creates a special 
## "matrix" object that can cache its inverse.
## The function creates list containing a function to
## 1. Set values of a matrix
## 2. Get matrix values
## 3. Set values of inverse matrix
## 4. Get inverse matrix values

makeCacheMatrix <- function(x = matrix()) {						## Input Parameter Matrix

  InvMatrix <-NULL												## Initialize Inverse Matrix value to null (For the first time)
  
  SetMatrix <- function(y) {									## "SetMatrix" function can be used to replace the existing matrix
    x <<- y														## by a new matrix.
    InvMatrix <<- NULL											## Inverse Matrix value is made null when original matrix is
  }																## replaced by a new matrix.
  
  GetMatrix <- function() x										## Returns the original matrix 
  
  InverseMatrix <- function(InvMat) InvMatrix <<-InvMat			## Stores the value of Matrix Inverse
  
  GetInverse <- function() InvMatrix							## Returns the Inverse Matrix
  
  list( SetMatrix = SetMatrix,									## Creates a list of functions
        GetMatrix = GetMatrix, 
        InverseMatrix = InverseMatrix, 
        GetInverse = GetInverse)
}
##############################################################################################################################
##############################################################################################################################
## "cacheSolve" function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), 
## then the "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {								## Input Parameter List with all "makeCacheMatrix"
																## functions
																
  InvMatrix <- x$GetInverse()									## Retrieves the value of Inverse  matrix
  
  if(!is.null(InvMatrix)) {										## If the inverse matrix is already cached
    print("Getting Cached Inverse Matrix")						## retrieve the value of inverse matrix from
    return(InvMatrix)											## cache, display or return the value and exist
  }																## out of the function.
  
  print("Caching Inverse Matrix")								## If the inverse matrix is not available in
  DataMatrix <- x$GetMatrix()									## in cache, get the original matrix, compute
  InvMatrix <- solve(DataMatrix)								## the value of inverse matrix and return the value
  x$InverseMatrix(InvMatrix)									## to "makeCacheMatrix" function via argument.
  
  InvMatrix														## Return a matrix that is the inverse of original matrix
}
##############################################################################################################################
##############################################################################################################################
