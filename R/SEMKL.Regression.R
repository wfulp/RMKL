SEMKL.regression=function(k,outcome,penalty,epsilon,tol=0.0001,max.iters=1000){
  n=length(outcome)
  A=c(rep(1,n),rep(-1,n))
  b=0
  r=0
  l=rep(0,2*n)
  c=c(epsilon-outcome,epsilon+outcome)
  u=rep(penalty,2*n)
  iters=0
  delta=rep(1,length(k))
  m=length(k)
  gamma=rep(1/m,m)
  gamma_all=list()
  #tic()
  while (max(delta)>tol && iters<max.iters){
    iters=iters+1
    gamma_all[[iters]]=gamma
    kk=Reduce('+',mapply("*", k, gamma,SIMPLIFY = FALSE))
    H=cbind(rbind(kk,-kk),rbind(-kk,kk))
    model=ipop(c,H,A,b,l,u,r)
    var=list('alpha'= primal(model)[1:n],'alphastar'= primal(model)[(n+1):(2*n)] )
    fnorm=sapply(1:length(k), function(a){
      sqrt(gamma[a]^2*((var$alpha-var$alphastar)%*%k[[a]]%*%(var$alpha-var$alphastar)))})
    temp=gamma
    gamma=fnorm/sum(fnorm)
    delta=abs(temp-gamma)
  }
  # toc(log=TRUE,quiet=TRUE)
  #time=tic.log(format=FALSE)[[1]]$toc-tic.log(format=FALSE)[[1]]$tic
  w.ipop=(var$alpha-var$alphastar)%*%kk
  R=which(penalty-var$alpha>10^(-7)&var$alpha>10^(-7))
  Rstar=which(penalty-var$alphastar>10^(-7)&var$alphastar>10^(-7))
  b.low=(-epsilon-outcome+w.ipop)[Rstar]
  b.up=(epsilon-outcome+w.ipop)[R]
  b.ipop=as.numeric(names(which.max(table(round(union(b.low,b.up),6)))))
  results=list("alpha"=var$alpha, "alpha.star"=var$alphastar, "gamma"=temp,
               "iters"=iters,'b'=b.ipop,'f'=w.ipop)
  return(results)
}
