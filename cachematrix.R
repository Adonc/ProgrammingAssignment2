## The two functions work on a Matrix,makeCacheMatrix takes a Matrix 
## caches it, this gets worked on by Cachsolve function, which returns its inverse 
## The two functions work on a Matrix,makeCacheMatrix takes a Matrix 
## caches it, this gets worked on by Cachsolve function, which returns its inverse 
## functions do

## Gets a Matrix and caches it, set_matrix sets the matrix, with the entered
##store_inves() stores the matrix inverse in cache memory, 
##get_inves_matrix returns the solved inverse from cache memory

makeCacheMatrix <- function(m_trix = matrix()) {
  inv_matrix<-NULL
  set_matrix<-function(x_matrix){
    m_trix<<-x_matrix
    inv_matrix<<- NULL
  }
  get_matrix<-function() m_trix
  store_inve<-function(new_inves_matrix) inv_matrix<<-new_inves_matrix
  get_inves_matrix<-function() inv_matrix
  
  list(get_matrix = get_matrix,store_inve = store_inve,get_inves_matrix = get_inves_matrix)
  

}


## returns the Inverse of the entered matrix, if it has already been solved,
## the cached inverse matrix is returned,otherwise its solved and stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix<-x$get_inves_matrix()
  if(!is.null(inverse_matrix)){
    message("retrieving cached matrix")
    inv_<-x$get_inves_matrix
    return(inverse_matrix)
  }else{
    mat_rix<-x$get_matrix()
    marix_<-solve(mat_rix)
    x$store_inve(solve(mat_rix))
    inv_<-x$get_inves_matrix()
    return(solve(mat_rix))}
  
}
