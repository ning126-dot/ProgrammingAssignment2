## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # 初始化缓存的逆矩阵为 NULL
  
  # 设置矩阵的函数
  set <- function(y) {
    x <<- y
    inv <<- NULL  # 每次更改矩阵时重置逆矩阵缓存
  }
  
  # 获取矩阵的函数
  get <- function() x
  
  # 设置逆矩阵的函数
  setInverse <- function(inverse) inv <<- inverse
  
  # 获取逆矩阵的函数
  getInverse <- function() inv
  
  # 返回包含四个方法的列表
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()  # 尝试获取缓存的逆矩阵
  
  # 如果逆矩阵已经被缓存，直接返回缓存值
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # 如果没有缓存逆矩阵，则计算逆矩阵
  data <- x$get()       # 获取矩阵
  inv <- solve(data, ...)  # 计算矩阵的逆
  x$setInverse(inv)     # 将逆矩阵存入缓存
  
  inv  # 返回逆矩阵
}
