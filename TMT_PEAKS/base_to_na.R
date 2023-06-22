base_to_NA <- function(patterns,dataset) {
  print("Zero values have been transformed to NAs")
  for (i in 1:length(patterns)){
    searching <- grep(pattern = patterns[[i]], names(dataset))
    dataset[searching][dataset[searching] <= 10000] <- NA}
  return(dataset)
}
