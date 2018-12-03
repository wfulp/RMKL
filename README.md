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
