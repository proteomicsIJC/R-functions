columns_checker <- function(dataset){
  print("Note that only those groups that will be printed will be those corresponding to samples and not pools")
  batching <- dataset %>% 
    filter(sample_or_pool == "sample")
  
  batching <- dcast(batching, 
                    protein_group ~ sample_name, value.var="normalized_intensity",
                    fun.aggregate = mean)
  batching <- batching[,-1]
  print(colnames(batching))
}
