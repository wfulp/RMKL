#' Converts Cost from DALMKL to SEMKL or SimpleMKL
#'
#' This function estimates an a comparable cost for SEMKL or SimpleMKL  from DALMKL.
#' @param ktrain Gramm matrix of training data 
#' @param DAl.model DAL MKL model 
#  @param C.convert Cost used in DAMKL model
#' @return C cost SEMKL or SimpleMKL 
#' @export

C.convert=function(K.train,DALMKL.model,C.DALMKL){
norm=function(x,y){y%*%x%*%y}
sum.norms=sum(sapply(1:length(K.train), function(a){
                 norm(K.train[[a]],DALMKL.model$alpha[,a])})
            C=C.DALMKL*sum.norms
            return(C)
}
)
)
}
