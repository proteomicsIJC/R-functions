#' Combine the expression profile of various technical replicates to create a single sample from them by median
#' @param dataset long format datase with a column named sample_name
#' @param technical_replicates column for the technical reps
#' @param sample_names  column for the real sample names
#' @param expression_unit  the grouping column for the entries of each sample (Protein.Group,Accession,Gene.names ...)
#' @param intensity_to_combine  column with the intensity to combine
#' @param other_intensities  column with the names of other intensities they will be removed if remove_reps is T
#' @param remove_reps  remove technical replicates column and also remove the other_intensities cols
#' @param report_results  whether if the process of the function should be reported or not in the used_parameters file
#' @export
sample_combination <- function(dataset,
                               technical_replicates = "", sample_names = "",
                               expression_unit = "", intensity_to_combine = "",
                               remove_reps = F, other_intensities = NULL,
                               report_results = T){
  if (report_results){
  cat("sample_combination",file = "./results/used_parameters.txt",sep = "\n", append = T)
  cat(paste0("Technical replicates have been combined by median calculation between samples from the same origin"), file = "./results/used_parameters.txt", sep = "\n", append = T)
  cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")}
  
  if (isTRUE(remove_reps)){
    dataset <- dataset %>% 
      group_by(get(sample_names),
               get(expression_unit)) %>% 
      mutate(combined_intensity = median(get(intensity_to_combine), na.rm = T)) %>% 
      subset(select = -c(get(technical_replicates), get(intensity_to_combine))) %>% 
      distinct()
    if (!is.null(other_intensities)){
      dataset <- dataset %>% 
        subset(select = -c(get(other_intensities))) %>% 
        distinct()}} 
  if (isFALSE(remove_reps)){
    dataset <- dataset %>% 
      group_by(get(sample_names),
               get(expression_unit)) %>% 
      mutate(combined_intensity = median(get(intensity_to_combine), na.rm = T))
  }
  dataset <- dataset[,!grepl("^get\\(",colnames(dataset))]
  return(dataset)
}




