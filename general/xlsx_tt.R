xlsx_tt <- function(fit__1, meta_data, meta_sample_column, meta_data_column, annoation, expression_matrix){
  ##### Definition of some thing i'll need <3
  ### meta data work
  rownames(meta_data) <- meta_data[,meta_sample_column]
  meta_data <- meta_data[,meta_data_column, drop= F]
  colnames(meta_data) <- "exp_group" 
  
  # meta data colors
  color_for_later <- c("#ff6699","#92d050","#00b0f0",
                       "#cc8128","#53c4b0","red")
  total_samples <- unique(meta_data$exp_group)
  color_sample <- as.data.frame(tibble::tibble(
    total_samples = total_samples,
    colorins = color_for_later[1:length(total_samples)]
  ))
  rownames(color_sample) <- color_sample$total_samples
  color_sample <- color_sample[,"colorins", drop = F]
  
  ### final merge
  meta_data$rows <- rownames(meta_data)
  meta_data <- merge(meta_data, color_sample,
                     by.x = "exp_group", by.y = "row.names")
  
  rownames(meta_data) <- meta_data$rows 
  meta_data <- meta_data[,c("exp_group","colorins")]
  
  ### TTs lists, tts and names  (coefficients)
  list_of_tts <- list()
  coefficients <- c(colnames(fit__1$coefficients))
  ##### TT extraction
  for (i in 1:length(colnames(fit1$coefficients))){
    top_table_unique <- topTable(fit = fit__1, coef = coefficients[i], number = Inf)
    # raw FC
    top_table_unique$Fold_Change <- ifelse(test = top_table_unique$logFC > 0, 
                                           yes = 2^top_table_unique$logFC,
                                           no = (-1/2^top_table_unique$logFC))
    # remove the non-wanted columns and reorder
    top_table_unique <- top_table_unique %>% 
      subset(select = -c(AveExpr,`F`,t,B))
    top_table_unique <- relocate(.data = top_table_unique, Fold_Change, .after = logFC)
    list_of_tts[[i]] <- top_table_unique}
  
  ### When only a TT is founf
  if (length(colnames(fit__1$coefficients)) == 1){
    remove(list_of_tts)
    list_of_tts <- list()
    top_table_unique <- topTable(fit = fit__1,number = Inf)
    # raw FC
    top_table_unique$Fold_Change <- ifelse(test = top_table_unique$logFC > 0, 
                                           yes = 2^top_table_unique$logFC,
                                           no = (-1/2^top_table_unique$logFC))
    # remove the non-wanted columns and reorder
    top_table_unique <- top_table_unique %>% 
      subset(select = -c(AveExpr,`F`))
    top_table_unique <- relocate(.data = top_table_unique, Fold_Change, .after = logFC)
    list_of_tts[[i]] <- top_table_unique
    names(list_of_tts) <- coefficients}
  
  ### When more than 1 is found
  else {
    names(list_of_tts) <- coefficients
    # Extract the ANOVA TT and process it 
    tt_to_add <- topTable(fit = fit__1, number = Inf)
    # remove the non-wanted columns and reorder
    tt_to_add <- tt_to_add %>%
      subset(select = -c(AveExpr,`F`))
    list_of_tts <- append(list("ANOVA" = tt_to_add),list_of_tts)} 
  
  ##### TTs final cleaning
  ## save names
  tt_names <- names(list_of_tts)
  
  #### Start looping through the list
  for (j in 1:length(list_of_tts)){
    if (tt_names[j] == "ANOVA"){
      next
    }
    ## Parsing rule for the contrasts
    both_samples <- tt_names[j]
    samples1 <- stringr::str_split(both_samples," vs. ", simplify = T)[1]
    samples2 <- stringr::str_split(both_samples," vs. ", simplify = T)[2]
    if (samples2 == "rest") {
      samples2 <- unique(meta_data$exp_group[!meta_data$exp_group %in% samples1])
    } else {
      samples2 <- samples2
    }
    
    ## meta data work
    meta_data_now <- meta_data %>% 
      filter(exp_group %in% c(samples1,samples2)) %>% 
      arrange(factor(exp_group, levels = c(samples1,samples2)))
    
    samples_now <- rownames(meta_data_now)[meta_data_now$exp_group %in% c(samples1,samples2)]
    ## expression matrix
    expression_matrix_now <- expression_matrix[,samples_now]
    list_of_tts[[j]] <- merge(list_of_tts[[j]], y = expression_matrix_now, by = "row.names")
    rows <- list_of_tts[[j]]$Row.names
    list_of_tts[[j]] <- list_of_tts[[j]] %>%
      relocate(all_of(colnames(expression_matrix_now)), .before = logFC) %>% 
      subset(select = -c(Row.names))
    rownames(list_of_tts[[j]]) <- rows
  }
  ## annotation work
  for (k in 1:length(list_of_tts)){
    list_of_tts[[k]] <- merge(annotation, list_of_tts[[k]], by = "row.names")
    # Clean the data
    colnames(list_of_tts[[k]])[1] <- "Accession"
    list_of_tts[[k]] <- list_of_tts[[k]][, !duplicated(colnames(list_of_tts[[k]]))]
    list_of_tts[[k]] <- list_of_tts[[k]] %>% 
      arrange(adj.P.Val)}
  tts <<- list_of_tts
  
  
  
  #### Time for the xlsx file
  wb <- createWorkbook("Ignasi Jarne Sanz")
  for (m in 1:length(list_of_tts)){
    ## Extract the datasets
    dataset <- list_of_tts[[m]]
    mi_nombre <- names(list_of_tts)[m]
    
    ## Add worksheet to the wb
    addWorksheet(wb, sheetName = mi_nombre)
    
    ## Write the data
    writeData(wb, sheet = m, x = dataset, 
              startCol = 1, startRow = 1, colNames = T)
    
    ## Add adjusted p value style
    
    ## Bold and right-center align all column names
    addStyle(wb, sheet = m, style = createStyle(textDecoration = "bold", halign = "left"), 
             rows = 1, cols = 1:ncol(dataset))
    
    ## Annotation style
    ##cols_of_annot <- colnames(annotation)
    ##annotation_col_indices <- sapply(cols_of_annot, function(col) which(names(dataset) == col))
    
    ##addStyle(wb, sheet = m, style = createStyle(border = "top", textDecoration = "bold"), 
    ##           rows = min(annotation_col_indices), cols = min(annotation_col_indices))
    
    ##addStyle(wb, sheet = m, style = createStyle(border = "top", textDecoration = "bold"), 
    ##           rows = 1:(nrow(dataset) + 1), cols = max(annotation_col_indices))
    
    ## ADD META DATA COLOR !!!!
    
  }
  saveWorkbook(wb = wb, "Top_Table_results.xlsx")
}
