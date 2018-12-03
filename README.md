# RMKL
Integrating multiple heterogeneous high throughput data sources is an emerging topic of interest in cancer research. Making decisions based upon metabolomic, genomic, etc. data sources can lead to better prognosis or diagnosis than simply using clinical data alone. Support vector machines (SVM) are not suitable for analyzing multiple data sources in a single analysis. SVM employs the kernel trick, thus it is able to construct nonlinear classification boundaries. However, obtaining an optimal kernel type, hyperparameter, and regularization parameter is a challenging task. Cross validation can be employed to make these selections. Ultimately, there may not be one single optimal kernel, but rather a convex combination of several kernel representations of the data. Methods that produce a classification rule based on a convex combination of candidate kernels are referred to as multiple kernel learning (MKL) methods

You can install RMKL from GitHub. If you already have a previous version of RMKL installed, you can use that to install the new version:

```{r}
install.packages("devtools")
library(devtools)
devtools::install_github("cwilso6/RMKL")
```
# Requirements
In order for RMKL to work properly, the following packages are required:
* caret
* kernlab

# Examples 
For our benchmark example, we have two groups which are drawn from a bivariate normal distribution where the mean of one groupis fixed and the group means shift to provide different amounts of overlap of the two groups.

## Loading data
```{r}
data(benchmark.data)
# The data sets are organized in a a list. Each entry of the list is a 100x3 matrix with each row consisting of a x- and y- coordinate, and a group label (-1,1).
#Below is a summary of the mean of each group for each mean structure.
lapply(1:length(data), function(a) aggregate(x = data[[1]][,1:2], by=list(data[[1]][,3]), mean))
```
## Using RMKL
```{r}
 data.mkl=data[[i]]
 kernels=rep('radial',2)
 sigma=c(2,1/20) 
 degree=sapply(1:length(kernels), function(a) ifelse(kernels[a]=='p',2,0))
 #Kernels.gen splts the data into a training and test set, and generates the desired kernel matrices.
 #Here we generate two gaussisan kernel matrices with sigma hyperparameter 2 and 0.05
 K=kernels.gen(data=data.mkl[,1:2],train.samples=train.samples,kernels=kernels,sigma=sigma,degree=degree,scale=rep(0,length(kernels)))
 C=0.05 #Cost parameter for DALMKL
 K.train=K$K.train
 K.test=K$K.test
  
  # parameters set up
  cri_outer = 0.01 # criterion for outer cycle, 0.01 is default by author
  cri_inner = cri_outer/10000  #criterion for inner cycle, this ratio is default by author
  calpha = 10 ### Lagrangian duality constraint parameter, must be positive, 10 is default by author
  max_iter_outer = 500 # maximum number of iterations in outer cycle
  max_iter_inner = 500 # maximum number of iterations in inner cycle
  ytr=data.mkl[train.samples,3]
  k.train=simplify2array(K.train) #Converts list of kernel matrices in to an array with is appropriate for C++ code
  k.test=simplify2array(K.test)
  
  #Implement DALMKL with the hinge loss function
  spicy_svmb1n=SpicySVM(k.train, ytr, C, cri_outer, cri_inner, max_iter_outer, max_iter_inner, calpha)
  spicysvmb1n_results=predictspicy(spicy_svmb1n$alpha,spicy_svmb1n$b, k = k.test)
  cm.DALMKL.svm=confusionMatrix(factor(sign(spicysvmb1n_results),levels=c(-1,1)), factor(data.mkl[-train.samples,3],levels=c(-1,1)))
  cm.DALMKL.svm
  
  #Implement DALMKL with a logistic loss function
  spicy_logib1n=SpicyLogit(k.train, ytr, C, cri_outer, cri_inner, max_iter_outer, max_iter_inner, calpha)
  spicylogib1n_results=predictspicy(spicy_logib1n$alpha,spicy_logib1n$b, k = k.test)
  cm.DALMKL.logi=confusionMatrix(factor(sign(spicylogib1n_results),levels=c(-1,1)), factor(data.mkl[-train.samples,3],levels=c(-1,1)))
  cm.DALMKL.logi
 
  #Convert C parameter from DALMKL implenetation to SimpleMKL and SEMKL implementation to make the four implementations comparible.
  C_SEMKL=C.convert(K.train,spicy_logib1n$model,C)
  
  #Implement SimpleMKL
  SimpleMKL.model=SimpleMKL.classification(k=K.train,data.mkl[train.samples,3], penalty=C_SEMKL)
  cm.SimpleMKL=confusionMatrix(factor(prediction.Classification(SimpleMKL.model,ktest=K.test,data.mkl[train.samples,3])$predict,       levels=c(-1,1)),factor(data.mkl[-train.samples,3],levels=c(-1,1)))
  cm.SimpleMKL
  
  #Implement SEMKL
  SEMKL.model=SEMKL.classification(k=K.train,data.mkl[train.samples,3], penalty=C_SEMKL)
  cm.SEMKL=confusionMatrix(factor(prediction.Classification(SEMKL.model,ktest=K.test,data.mkl[train.samples,3])$predict,
  levels=c(-1,1)),factor(data.mkl[-train.samples,3],levels=c(-1,1)))
  cm.SEMKL
  
  
