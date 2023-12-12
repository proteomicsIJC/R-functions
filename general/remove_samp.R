remove_samp <- function(dataset, samples=NULL, reasons=NULL){
  if(!is.null(samples)){
    print("Removing indicated samples")
    cat("remove_samp", file = "./results/used_parameters.txt",append = T, sep = "\n")
    for (i in 1:length(samples)){
      dataset <- subset(dataset, sample_name!=samples[i])
      cat(paste0("The following sample has been removed: ",samples," Reason for the removal: ",reasons[i]), 
          file = "./results/used_parameters.txt",append = T, sep = "\n")
    }
    cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")
  }
  if (is.null(samples)){
    print("No sample will be removed")
    dataset <- dataset
    cat("remove_samp",file = "./results/used_parameters.txt",append = T, sep = "\n")
    cat("No samples have been removed", file = "./results/used_parameters.txt",append = T, sep = "\n")
    cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")
  }
  return(dataset)
}
