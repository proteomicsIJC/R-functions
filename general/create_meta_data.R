create_meta_data <- function(group_matrix_dataset, what_is_your_tmt){
  print("Creating metadata dataset")
  numbers <- as.vector(t(group_matrix_dataset))
  numbers <- gsub(" ", "", numbers)
  for (k in 1:length(numbers)){
    if (!startsWith(x = numbers[k], prefix = "POOL")){
      numbers[k] <- paste("sample",numbers[k],sep = "_")  
    }
  }
  empty <- c()
  list_of_tmts <- list()
  final_vector <- list()
  for (j in 1:length(rownames(group_matrix_dataset))){
    plexes <- rownames(group_matrix_dataset[j])
    for (i in 1:length(colnames(group_matrix_dataset))){
      tmt_version <- colnames(group_matrix_dataset[i])
      tmt_versions <- c(empty,tmt_version)
      list_of_tmts[[i]] <- tmt_versions
      list_of_tmts <- unlist(list_of_tmts)
      the_names <- paste0(plexes[j],"_",what_is_your_tmt,"_",list_of_tmts)
    }
    
    final_vector[[j]] <- the_names
  }
  final_vector <- unlist(final_vector)
  name_number <- data.frame("sample_number" = numbers,
                            "sample_name" = final_vector)
  return(name_number)
}
