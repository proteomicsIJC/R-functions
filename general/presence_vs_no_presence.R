presence_vs_no_presence <- function(contrasts_vector,list){
  final_names <- contrasts_vector
  final_list <- list()
  for (i in 1:length(contrasts_vector)){
    
    # Samples to merge
    both_samp <- final_names[i]
    dataset1_name <- str_split(both_samp, "_vs_", simplify = T)[1]
    dataset2_name <- str_split(both_samp, "_vs_", simplify = T)[2]
    
    dataset1 <- list[[dataset1_name]]
    dataset2 <- list[[dataset2_name]]
    
    # final merge
    merged <- merge(dataset1, dataset2, by = "protein_group")
    merged <- merge(merged, annotation, by = "protein_group")
    
    # add TT to the list 
    final_list[[i]] <- merged
  }
  names(final_list) <- final_names
  return(final_list)
}
