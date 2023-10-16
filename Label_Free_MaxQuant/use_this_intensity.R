use_this_intensity <- function(dataset,pattern){
  intensities <- grep(pattern = pattern, colnames(dataset))
  major <- grep(pattern = "Majority protein IDs", colnames(dataset))
  dataset <- dataset[,c(c(major),
                        ####c(intensities))]
}