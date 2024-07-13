#' Function to transform 0 to NAs to a series of columns 
#' @param  patterns a vector of sample names 
#' @param dataset a dataset in "expression matrix" format with colnames as sample names
#' @export 
zero_to_NA_label_free <- function(patterns,dataset) {
  print("Zero values have been transformed to NAs")
  for (i in 1:length(patterns)){
    searching <- grep(pattern = paste0("^",patterns[[i]],"$", collapse = ""), names(dataset))
    dataset[searching][dataset[searching] == 0] <- NA}
  return(dataset)
}
