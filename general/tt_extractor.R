tt_extractor <- function(fit1,annotation){
  coefficients <- colnames(head(fit1@.Data)[[1]])
  tt_list <- list()
  for (i in 1:length(colnames(head(fit1@.Data)[[1]]))){
    no_pg <- which(colnames(annotation) != "protein_group")
    no_pg <- colnames(annotation)[no_pg]
    top_table_unique <- topTable(fit = fit1, coef = coefficients[[i]], number = Inf)
    top_table_unique <- merge(top_table_unique, annotation, by = "row.names")
    colnames(top_table_unique)[1] <- "protein_group"
    top_table_unique <- top_table_unique[, !duplicated(colnames(top_table_unique))]
    top_table_unique <- relocate(.data = top_table_unique, all_of(no_pg), .after = protein_group)
    tt_list[[i]] <- top_table_unique
  }
  names(tt_list) <- coefficients
  names(tt_list) <- gsub(pattern = " ", replacement = "_", x = names(tt_list))
  return(tt_list)
}
