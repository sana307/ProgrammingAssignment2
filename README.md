ProgrammingAssignment2

Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

I wrote a pair of functions that cache the inverse of a matrix:

makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
