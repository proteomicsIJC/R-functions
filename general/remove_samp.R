#' Remove a sample or a group of samples
#' @param dataset long format datase with a column named sample_name
#' @param samples vector of names of the samples to remove
#' @param reasons vector for the reasons why each of the removed smaples are removed from the analysis
#' @param report_results make it or not able to write in the used_parameters.txt file
#' @export
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
