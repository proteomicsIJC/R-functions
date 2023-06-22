log2_to_pattern <- function(patterns,dataset){
  print("log 2 transformation of the intensity values")
  print("Note that, negative values after transformation have been assigned to NA")
  cat("log2 transformation",file = "./results/used_parameters.txt",sep = "\n",append = T)
  cat(paste0("Intensity values have been log2 transformed and negative values after transformation have been assigned to NA"), file = "./results/used_parameters.txt", sep = "\n", append = T)
  cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")
  for (i in 1:length(patterns)){
    searching <- grep(pattern = patterns[[i]], names(dataset))
    dataset[searching] <- log2(dataset[searching])
    dataset[searching][dataset[searching] <= 0] <- NA}
  return(dataset)
}
