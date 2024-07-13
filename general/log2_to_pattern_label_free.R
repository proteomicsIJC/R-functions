#' Function to apply log2 to a series of columns 
#' @param patterns a parsing rule of sample names to make the log2 conversion
#' @param dataset a dataset in "expression matrix" format with colnames as sample names 
#' @param report_results make it or not able to write in the used_parameters.txt file
#' @export
log2_to_pattern_label_free <- function(patterns,dataset,report_results = T){
  print("log 2 transformation of the intensity values")
  if (report_results){
  print("Note that, negative values after transformation have been assigned to NA")
  cat("log2 transformation",file = "./results/used_parameters.txt",sep = "\n",append = T)
  cat(paste0("Intensity values have been log2 transformed and negative values after transformation have been assigned to NA"), file = "./results/used_parameters.txt", sep = "\n", append = T)
  cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")}
  for (i in 1:length(patterns)){
    searching <- grep(pattern = paste0("^",patterns[[i]],"$", collapse = ""), names(dataset))
    dataset[searching] <- log2(dataset[searching])
    dataset[searching][dataset[searching] <= 0] <- NA}
  return(dataset)
}
