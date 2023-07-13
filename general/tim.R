tim <- function(impute,dataset,NAs_prop){
  # PGs that have some NA
  for_nas <- dataset
  cols <- c("protein_group")
  for_nas <- for_nas %>%
    group_by(across(all_of(cols))) %>%
    mutate(count_NA = sum(is.na(normalized_intensity))) %>%
    ungroup()
  for_nas <- for_nas %>%
    filter(count_NA > 0)
  pg_with_nas <<- for_nas
  
  # Only NAs values
  only_nas <<- dataset[!complete.cases(dataset),]
  n <- length(unique(dataset$sample_name))
  if (impute == "no"){
    print("No missing values imputation is performed, all PGs with more than 0 NA values will be removed")
    cols <- c("protein_group")
    dataset <- dataset %>%
      group_by(across(all_of(cols))) %>%
      mutate(count_NA = sum(is.na(normalized_intensity))) %>%
      ungroup()
    
    # Filter pg with less than NAs_prop
    dataset <- dataset %>%
      filter(count_NA == 0)
    
    # Filter pg with less than NAs_prop
    dataset <- dataset %>% 
      group_by(across(all_of(cols))) %>%
      filter(n() >= length(unique(sample_name))) %>%
      ungroup()
    imputed_data <- dataset
    cat("tim - for NA imputation",file = "./results/used_parameters.txt",sep = "\n", append = T)
    cat(paste0("impute == no"), file = "./results/used_parameters.txt", sep = "\n", append = T)
    cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")
  }
  if (impute == "yes"){
    print("Missing values will be imputed using mice. A maximum proportion of ")
    print(paste0("A maximum proportion of ",NAs_prop," is accepted"))
    cols <- c("protein_group","exp_group")
    dataset <- dataset %>%
      group_by(across(all_of(cols))) %>%
      mutate(count_NA = sum(is.na(normalized_intensity))) %>%
      mutate(per_NA = sum(is.na(normalized_intensity)) /sum(sum(is.na(normalized_intensity)) + sum(!is.na(normalized_intensity)))) %>%
      ungroup() %>%
      mutate(was_NA = ifelse(is.na(normalized_intensity) == T,"Was NA","Was not NA"))
    
    # Filter pg with less than NAs_prop
    dataset <- dataset %>%
      filter(per_NA <= NAs_prop)
    
    # Reshape the data to impute
    to_imput <- reshape2::dcast(dataset, 
                      protein_group ~ sample_name, value.var="normalized_intensity", fun.aggregate = median)
    
    rownames(to_imput) <- to_imput$protein_group
    to_imput <- to_imput[,-1]
    
    # Impute the data
    imputed_data <- complete(mice(to_imput, m = 5, method = "pmm", seed = 500))
    
    # Reshape the data again to long format
    samples <- rep(colnames(imputed_data), each = length(unique(rownames(imputed_data))))
    pgs <- rep(rownames(imputed_data), times = length(unique(samples)))
    values <- unlist(as.vector(imputed_data), use.names = F)
    
    imputed_data_long <- data_frame("sample_name" = samples,
                                    "protein_group" = pgs,
                                    "normalized_intensity" = values)
    
    dataset[,"normalized_intensity_before_imputation"] <- dataset[,"normalized_intensity"]
    dataset <- subset(dataset, select = -c(normalized_intensity))
    
    imputed_data <- merge(imputed_data_long, dataset, by = c("sample_name","protein_group"))
    imputed_data <- imputed_data %>%
      relocate(normalized_intensity, .after = intens)
    
    cols <- c("protein_group")
    imputed_data <- imputed_data %>%
      group_by(across(all_of(cols))) %>%
      filter(n() == n) %>%
      ungroup()
    cat("tim - for NA imputation",file = "./results/used_parameters.txt",sep = "\n", append = T)
    cat(paste0("impute == yes ",unique(NAs_prop)), file = "./results/used_parameters.txt", sep = "\n", append = T)
    cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")
  }
  return(imputed_data)
}
