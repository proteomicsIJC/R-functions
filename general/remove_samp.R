###################
### remove_samp ###
###################

## dataset = long format dataframe with sa sample_name column
## samples = samples to remove (if nothing is specified the function doesn't remove anything)
## reasons = optional, vector with the reasons to remove the samples
## report_results = make it or not able to write in the used_parameters.txt file

remove_samp <- function(dataset, samples=NULL, reasons=NULL, report_results= T){
  if(!is.null(samples)){
    print("Removing indicated samples")
    if (report_results){
    cat("remove_samp", file = "./results/used_parameters.txt",append = T, sep = "\n")}
    for (i in 1:length(samples)){
      dataset <- subset(dataset, sample_name!=samples[i])
      cat(paste0("The following sample has been removed: ",samples[i]," Reason for the removal: ",reasons[i]), 
          file = "./results/used_parameters.txt",append = T, sep = "\n")
    }
    cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")
  }
  if (is.null(samples)){
    print("No sample will be removed")
    dataset <- dataset
    if (report_results){
    cat("remove_samp",file = "./results/used_parameters.txt",append = T, sep = "\n")
    cat("No samples have been removed", file = "./results/used_parameters.txt",append = T, sep = "\n")
    cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")}
  }
  return(dataset)
}
