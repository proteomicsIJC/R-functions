tt_list_cleaner <- function(list, meta_data){
  final_names <- names(list)
  final_list <- list()
  for (i in 1:length(list)){
    cleanedtt <- as.data.frame(list[[i]])
    cleanedtt <- subset(cleanedtt, select = -c(AveExpr,t,B))
    #cleanedtt$Fold_Change <- 2^cleanedtt$logFC
    cleanedtt$Fold_Change <- ifelse(test = cleanedtt$logFC > 0, 
                                    yes = 2^cleanedtt$logFC,
                                    no = (-1/2^cleanedtt$logFC))
    cleanedtt <- relocate(.data = cleanedtt, Fold_Change, .after = logFC)
    
    # Get the experimental group names
    both_samp <- final_names[i]
    sample_1 <- str_split(both_samp, "_vs_", simplify = T)[1]
    sample_2 <- str_split(both_samp, "_vs_", simplify = T)[2]
    
    # Corresponding to meta_data 2
    meta_data2 <- meta_data[meta_data$sample_name %in% meta_data_tracker$sample_name,]
    
    # Correspondance
    correspond1 <- meta_data2$sample_name[meta_data2$exp_group == sample_1]
    correspond2 <- meta_data2$sample_name[meta_data2$exp_group == sample_2]
    correspond <- c(correspond1,correspond2)
    
    # Corresponding to meta_data 2
    meta_data2 <- meta_data2[meta_data2$sample_name %in% correspond,]
    
    # Get the data
    to_bind <- as.data.frame(batching)
    to_bind <- to_bind[colnames(to_bind) %in% c(correspond1,correspond2)]
    to_bind <- to_bind %>% 
      select(all_of(correspond))
    to_bind$protein_group <- rownames(to_bind)
    
    # final merge
    cleanedtt <- merge(to_bind, cleanedtt, by = "protein_group")
    cleanedtt <- cleanedtt[order(cleanedtt$adj.P.Val),]
    no_pg <- which(colnames(annotation) != "protein_group")
    no_pg <- colnames(annotation)[no_pg]
    cleanedtt <- cleanedtt %>%
      select(c(protein_group,all_of(no_pg)), everything())
    
    # col_name_change
    old_names <- colnames(cleanedtt)[colnames(cleanedtt) %in% meta_data_tracker$sample_name]
    new_names <- c()
    
    for ( k in 1:length(old_names)){
      grup <- meta_data$exp_group[meta_data$sample_name == old_names[k]]
      sample_number <- meta_data$sample_number[meta_data$sample_name == old_names[k]]
      sample_number <- gsub(x = sample_number, "sample_","")
      new_names[k] <- paste(grup,sample_number,sep = ".")
    }
    
    
    name_changer <- data.frame(old = old_names,
                               new = new_names)
    
    setnames(cleanedtt, name_changer$old, name_changer$new)
    
    # add TT to the list 
    final_list[[i]] <- cleanedtt
  }
  names(final_list) <- final_names
  return(final_list)
}
