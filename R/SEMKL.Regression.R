#' Simple and Efficient MKL 
#'
#' This function conducts Simple and Efficnent MKL for precomputed gramm matrices
#' @param k list of Gramm matrices
#' @param outcome vector of binary outcome -1 and 1
#' @param penalty penalty of the smoothness of the resulting desicion rules
#' @param tol change between to iterations is smaller than this, algorithms is considered to have converged
#' @param max.iters maximum number of allowed iteratons
#' @return gamma weight vector for the importnace of each kernel 
#' @return alpha,alpha.star coeffiencents of the dual of MKL
#' @return time total amount of time to train model
#' @return iters Numvber of iterations to reach convergence criteria
#' @export

SEMKL.regression=function(k,outcome,penalty,epsilon,tol=0.0001,max.iters=1000){
  n=length(outcome)
  A=c(rep(1,n),rep(-1,n))
  b=0
  r=0
  l=rep(0,2*n)
  c=c(epsilon-data$y,epsilon+data$y)
  u=rep(penalty,2*n)
  iters=0
  delta=rep(1,length(k))
  m=length(k)
  gamma=rep(1/length(kernels),length(kernels))
  gamma_all=list()
  tic()
  while (max(delta)>tol && iters<max.iters){
    iters=iters+1
    gamma_all[[iters]]=gamma
    kk=Reduce('+',mapply("*", k, gamma,SIMPLIFY = FALSE))
    H=cbind(rbind(kk,-kk),rbind(-kk,kk))
    model=ipop(c,H,A,b,l,u,r)
    var=list('alpha'= primal(model)[1:n],'alphastar'= primal(model)[(n+1):(2*n)] )
    fnorm=sapply(1:length(kernels), function(a){
                 sqrt(gamma[a]^2*((var$alpha-var$alphastar)%*%k[[a]]%*%(var$alpha-var$alphastar)))})
    temp=gamma
    gamma=fnorm/sum(fnorm)
    delta=abs(temp-gamma)
    }
  toc(log=TRUE,quiet=TRUE)
  time=tic.log(format=FALSE)[[1]]$toc-tic.log(format=FALSE)[[1]]$tic
  w.ipop=(var$alpha-var$alphastar)%*%kk
  R=which(penalty-var$alpha>10^(-7)&var$alpha>10^(-7))
  Rstar=which(penalty-var$alphastar>10^(-7)&var$alphastar>10^(-7))
  b.low=(-epsilon-data$y+w.ipop)[Rstar]
  b.up=(epsilon-data$y+w.ipop)[R]
  b.ipop=as.numeric(names(which.max(table(round(union(b.low,b.up),6)))))
  results=list("alpha"=var$alpha, "alpha.star"=var$alphastar, "gamma"=temp,
                       "iters"=iters,"time"=time,'b'=b.ipop,'f'=w)
  return(results)
}

