#' Generate benchmark datasets
#'
#' This function generates bivariate normal data from two groups, where 
#' each group a specified mean.
#' @param n number of observations per group 
#' @param mean 2x2 matrix with means of the two gtoups  
#' @param sigma Covariance matrix of the two groups
#' @return x coordinates of the observations
#; @return group which of the two groups an observation belongs to  
#' @export 

benchmark.data.generation=function(n, mu, sigma){
  group=function(n,mean,sigma){rmvnorm(n,mean,sigma)}
  g1=cbind(group(n,mu[1,], sigma[[1]]),rep(-1,n),rep(1,n))
  g2=cbind(group(n,mu[1,], sigma[[1]]),rep(-1,n),rep(2,n))
  g3=cbind(group(n,mu[2,], sigma[[2]]),rep(1,n),rep(3,n))
  g4=cbind(group(n,mu[2,], sigma[[2]]),rep(1,n),rep(4,n))
  data=rbind(g1,g2,g3,g4)
  
  return(cbind('x'=data[,1:2],'group'=data[,3]))
}




