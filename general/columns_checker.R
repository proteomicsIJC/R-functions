#' Create a contrast matrix with all possible contrasts given a design matrix 
#' @param dataset a long format dataset with a column named sample_name
#' @param unit_of_analysis the unit of the analysis
#' @param intensity the column name of the intensity information that is going to be use
#' @param tmt TRUE of FALSE, if TRUE, it is expected that the analysis is done on TMT samples and expects a column named
#' sample_or_pool, where "sample" indicates a sample and POOL indicates that that sample is really a POOL
#' @export
columns_checker <- function(dataset,
                            unit_of_analysis = "Protein.Group",intensity = "normalized_intensity",
                            tmt = F){
  print("Note that only those groups that will be printed will be those corresponding to samples and not pools")
  if (isFALSE(tmt)){
  batching <- dataset}
  if (isTRUE(tmt)){
    batching <- dataset %>% 
    filter(sample_or_pool == "sample")}
  
  batching <- reshape2::dcast(batching, 
                    get(unit_of_analysis) ~ sample_name, value.var = intensity,
                    fun.aggregate = mean)
  batching <- batching[,-1]
  print(colnames(batching))
}
