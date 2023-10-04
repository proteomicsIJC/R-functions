zero_to_NA <- function(patterns,dataset) {
  print("Zero values have been transformed to NAs")
  for (i in 1:length(patterns)){
    searching <- grep(pattern = paste0("^",patterns[[i]],"$", collapse = ""), names(dataset))
    dataset[searching][dataset[searching] == 0] <- NA}
  return(dataset)
}
