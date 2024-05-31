#######################
### log2_to_pattern ###
#######################

## patterns = a pattern (parsing rule) of sample names to make the log2 conversion
## dataset = a dataset in "expression matrix" format with colnames as sample names 
## report_results = make it or not able to write in the used_parameters.txt file

log2_to_pattern <- function(patterns,dataset, report_results = T){
  if (report_results){
    print("log 2 transformation of the intensity values")
    print("Note that, negative values after transformation have been assigned to NA")
    cat("log2 transformation",file = "./results/used_parameters.txt",sep = "\n",append = T)
    cat(paste0("Intensity values have been log2 transformed and negative values after transformation have been assigned to NA"), file = "./results/used_parameters.txt", sep = "\n", append = T)
    cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")}
  for (i in 1:length(patterns)){
    searching <- grep(pattern = patterns[[i]], names(dataset))
    dataset[searching] <- log2(dataset[searching])
    dataset[searching][dataset[searching] <= 0] <- NA}
  return(dataset)
}
