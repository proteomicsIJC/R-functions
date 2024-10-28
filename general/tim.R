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
tim <- function(impute,dataset,
                NAs_prop, experimental_groups = NULL,
                intensity_to_impute = "", unit_to_impute = "Protein.Group",
                presence_no_presence = F,  
                report_results = T){
  
  ### Report results
  if (report_results){
    if (impute == "yes"){
      cat("tim - for NA imputation",file = "./results/used_parameters.txt",sep = "\n", append = T)
      cat(paste0("impute == yes ",NAs_prop), file = "./results/used_parameters.txt", sep = "\n", append = T)
      cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")}
    else if (impute == "no"){
      cat("tim - for NA imputation",file = "./results/used_parameters.txt",sep = "\n", append = T)
      cat(paste0("impute == no"), file = "./results/used_parameters.txt", sep = "\n", append = T)
      cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")}
  }
  
  # meta_data for the imputation
  # if it is NULL. all samples come from the same group
  if(is.null(experimental_groups)){
    meta_data_for_imputation <- tibble::tibble(
      sample_name = unique(dataset$sample_name),
      exp_group = rep("Ungrouped", times = length(unique(dataset$sample_name)))
    )
    dataset <- merge(dataset, meta_data_for_imputation, by = "sample_name")
  }
  
  if(!is.null(experimental_groups)){
    meta_data_for_imputation <- dataset %>% 
      subset(select = c(sample_name, get(experimental_groups))) %>% 
      distinct()
    colnames(meta_data_for_imputation)[2] <- "exp_group"
    dataset <- merge(dataset, meta_data_for_imputation, by = "sample_name")
  }
  
  ### NO Nas (impute == "no")
  # Remove all values with more than 0 NAs
  if (impute == "no"){
    dataset <- dataset %>%
      group_by(get(unit_to_impute)) %>% 
      mutate(number_of_NAs = sum(is.na(get(intensity_to_impute)))) %>% 
      ungroup() %>% 
      filter(number_of_NAs == 0) %>% 
      subset(select = -c(number_of_NAs, `get(unit_to_impute)`)) 
  }
  
  ### Some NAs (impute == "yes")
  if (impute == "yes"){
    n_of_samples <- length(meta_data_for_imputation$sample_name)
    # Remove values with a percentage of NAs bigger than NAs_prop
    dataset <- dataset %>% 
      group_by(get(unit_to_impute), exp_group) %>% 
      mutate(possible_values = n()) %>% 
      mutate(number_of_NAs = sum(is.na(get(intensity_to_impute)))) %>%
      mutate(per_NA = number_of_NAs/possible_values) %>% 
      ungroup() %>% 
      filter(per_NA <= NAs_prop) %>% 
      subset(select = -c(`get(unit_to_impute)`, possible_values)) %>%
      group_by(get(unit_to_impute)) %>% 
      mutate(number_of_vals = n()) %>% 
      filter(number_of_vals == n_of_samples) %>% 
      ungroup() %>% 
      subset(select = -c(`get(unit_to_impute)`, number_of_vals))
    # Reshape the data to impute
    to_imput <- reshape2::dcast(dataset,
                                get(unit_to_impute) ~ sample_name, value.var= intensity_to_impute, fun.aggregate = median)
    rownames(to_imput) <- to_imput[,1]
    to_imput <- to_imput[,-1]
    imputed_data <- complete(mice(to_imput, m = 5, method = "pmm", seed = 500))
    # Reshape the data again to long format
    samples <- rep(colnames(imputed_data), each = length(unique(rownames(imputed_data))))
    pgs <- rep(rownames(imputed_data), times = length(unique(samples)))
    values <- unlist(as.vector(imputed_data), use.names = F)
    
    imputed_data_long <- as.data.frame(tibble::tibble("sample_name" = samples,
                                                      unit_to_impute = pgs,
                                                      imputed_intensity = values))
    colnames(imputed_data_long)[2] <- unit_to_impute
    imputed_data <- merge(imputed_data_long, dataset, by = c("sample_name",unit_to_impute))
    imputed_data <- imputed_data %>%
      relocate(imputed_intensity, .after = all_of(intensity_to_impute))
    dataset <- imputed_data
  }
  return(dataset)
}
