##################
### zero_to_NA ###
##################

## patterns = a vector of sample names, this is a relaxed search so if all samples start by "sample_" we can specify only sample_ and the operation will work on all samples 
## dataset = a dataset in "expression matrix" format with colnames as sample names 

zero_to_NA <- function(patterns,dataset) {
  print("Zero values have been transformed to NAs")
  for (i in 1:length(patterns)){
    searching <- grep(pattern = patterns[[i]], names(dataset))
    dataset[searching][dataset[searching] == 0] <- NA}
  return(dataset)
}
