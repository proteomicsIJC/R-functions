#######################
### columns_checker ###
#######################

## dataset = A long format dataframe with a column named sample_name and a second column named normalized_intensity  

columns_checker <- function(dataset){
  print("Note that only those groups that will be printed will be those corresponding to samples and not pools")
  batching <- dataset %>% 
    filter(sample_or_pool == "sample")
  
  batching <- reshape2::dcast(batching, 
                    protein_group ~ sample_name, value.var="normalized_intensity",
                    fun.aggregate = mean)
  batching <- batching[,-1]
   ###### HOLA !!!! SEGONA PROVA
  print(colnames(batching))
}
