#' Using mice impute missing values with less than a proportion of NAs in all experimental groups
#' @param dataset long format datase with a column named sample_name
#' @param technical_replicates column for the technical reps
#' @param sample_names  column for the real sample names
#' @param impute  yes or no, whether if we want to impute the data or not
#' @param dataset  a long format dataframe: it requires to have a intensity column, a proteim_group column and a exp_group column that will define the NAs_prpp proportion
#' and also a sample_name column
#' @param NAs_prop  the max proportion of NA values per group to consider the nimber to enter the imputation
#' @param presence_no_presence  to get all the data with all the missing values
#' @param intensity_to_impute  colname of the intensity we want to apply the imputation on
#' @param report_results  in case we have to save the results or not
#' @export
tim <- function(impute,dataset,NAs_prop,presence_no_presence = F, report_results = T, intensity_to_impute = "", 
                unit_to_impute = "Protein.Group", experimental_groups = NULL){
  # meta_data for the imputation
  # if it is NULL. all samples come from the same group
  if(is.null(experimental_groups)){
    meta_data_for_imputation <- as.data.frame(tibble::tibble(
      sample_name = unique(dataset$sample_name),
      exp_group = rep("Ungrouped", times = length(unique(dataset$sample_name)))
      ))
    dataset <- merge(dataset, meta_data_for_imputation, by = "sample_name")
  }
  
  if(!is.null(experimental_groups)){
    meta_data_for_imputation <- as.data.frame(tibble::tibble(
      sample_name = names(experimental_groups),
      exp_group = experimental_groups
      ))
    dataset <- merge(dataset, meta_data_for_imputation, by = "sample_name")
  }
  
  # PGs that have some NA
  for_nas <- dataset
  cols <- c(unit_to_impute)
  for_nas <- for_nas %>%
    group_by(across(all_of(cols))) %>%
    mutate(count_NA = sum(is.na(get(intensity_to_impute)))) %>%
    ungroup()
  for_nas <- for_nas %>%
    filter(count_NA > 0)
  pg_with_nas <<- for_nas
  
  # Only NAs values
  only_nas <<- dataset[!complete.cases(dataset),]
  n <- length(unique(dataset$sample_name))
  if (impute == "no"){
    print("No missing values imputation is performed, all PGs with more than 0 NA values will be removed")
    cols <- c(unit_to_impute)
    dataset <- dataset %>%
      group_by(across(all_of(cols))) %>%
      mutate(count_NA = sum(is.na(get(intensity_to_impute)))) %>%
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
    if (report_results){
    cat("tim - for NA imputation",file = "./results/used_parameters.txt",sep = "\n", append = T)
    cat(paste0("impute == no"), file = "./results/used_parameters.txt", sep = "\n", append = T)
    cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")}
  }
  if (impute == "yes"){
    print("Missing values will be imputed using mice. A maximum proportion of ")
    print(paste0("A maximum proportion of ",NAs_prop," is accepted"))
    cols <- c(unit_to_impute,"exp_group")
    dataset <- dataset %>%
      group_by(across(all_of(cols))) %>%
      mutate(count_NA = sum(is.na(get(intensity_to_impute)))) %>%
      mutate(per_NA = sum(is.na(get(intensity_to_impute))) /sum(sum(is.na(get(intensity_to_impute))) + sum(!is.na(get(intensity_to_impute))))) %>%
      ungroup() %>%
      mutate(was_NA = ifelse(is.na(get(intensity_to_impute)) == T,"Was NA","Was not NA"))
    
    # Filter pg with less than NAs_prop
    dataset <- dataset %>%
      filter(per_NA <= NAs_prop)
    
    # Reshape the data to impute
    to_imput <- reshape2::dcast(dataset, 
                                get(unit_to_impute) ~ sample_name, value.var= intensity_to_impute, fun.aggregate = median)
    
    rownames(to_imput) <- to_imput[,1]
    to_imput <- to_imput[,-1]
    
    # Impute the data
    imputed_data <- complete(mice(to_imput, m = 5, method = "pmm", seed = 500))
    
    # Reshape the data again to long format
    samples <- rep(colnames(imputed_data), each = length(unique(rownames(imputed_data))))
    pgs <- rep(rownames(imputed_data), times = length(unique(samples)))
    values <- unlist(as.vector(imputed_data), use.names = F)
    
    imputed_data_long <- data_frame("sample_name" = samples,
                                    unit_to_impute = pgs,
                                    imputed_intensity = values)
    colnames(imputed_data_long)[2] <- unit_to_impute
    #####dataset[,"normalized_intensity_before_imputation"] <- dataset[,intensity_to_impute]
    #####    dataset <- subset(dataset, select = -c(normalized_intensity))
    
    imputed_data <- merge(imputed_data_long, dataset, by = c("sample_name",unit_to_impute))
    imputed_data <- imputed_data %>%
      relocate(imputed_intensity, .after = all_of(intensity_to_impute))
    
    cols <- c(unit_to_impute)
    imputed_data <- imputed_data %>%
      group_by(across(all_of(cols))) %>%
      filter(n() == n) %>%
      ungroup()
    if (report_results){
    cat("tim - for NA imputation",file = "./results/used_parameters.txt",sep = "\n", append = T)
    cat(paste0("impute == yes ",unique(NAs_prop)), file = "./results/used_parameters.txt", sep = "\n", append = T)
    cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")}
  }
  if (presence_no_presence == T){
    cat("Returning an expression matrix with all NAs to do presence/No presence analysis")
    to_imput <- reshape2::dcast(dataset, 
                                get(unit_to_impute) ~ sample_name, value.var= intensity_to_impute, fun.aggregate = median)
    
    rownames(to_imput) <- to_imput[,1]
    to_imput <- to_imput[,-1]
    expression_mat_presence_no_presence <<- to_imput
    imputed_data <- "no data to be displayed as no imputation is done and this procedure is only done in order to not apply any statistics"
  }
  
  return(imputed_data)
}
