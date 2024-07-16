#' Trasnform limma results into an excel file
#' @param  fit__1 limma fit object with its expression matrix and the contrasts done
#' @param meta_data meta_data dataframe with rownames assigned to sample names
#' @param meta_sample_column a column name of the meta_data dataset where the sample names are specified
#' @param meta_data_column a column name of the meta_data dataset where the groups of the samples are specified
#' @param annotation annotation for the rownames of the expression matrix, it needs to have a column to be named Accession which will have to correspond to the rownames of the expression matrix,
#' also, rownames of the annotation dataframe shall correspond to the Accession column
#' @param expression_matrix expression matrix with colnames as sample names as in the meta_data object and rownames as the rownames of the annotation rownames
#' @param filename the file name of the xlsx writen file, if it already exists it will crash
#' @param color_samples a named vector with names as group names and values as colors, colours shall be in hex value or be a color name recognized by R
#' @param differentiating_element the character element that differentiate the elements of the contrast, preset is " vs. "
#' ej. For a comparison named group1 vs. group3 differentiate element is " .vs " with the spaces !!!!!
#'     For a comparison named group1_vs_group3 differentiate element is "_vs_"  wichout the spaces if no spaces are in the contrast matrix !!!
#' @export
xlsx_tt <- function(fit__1, meta_data, meta_sample_column, meta_data_column, annotation, expression_matrix, filename = "TT_res.xlsx",
                    color_samples = NULL, differentation_element = " vs. ", more_than_a_group_per_sample = F){
  ##### Definition of some thing i'll need <3
  ### meta data work
  rownames(meta_data) <- meta_data[,meta_sample_column]
  meta_data <- meta_data[,meta_data_column, drop= F]
  colnames(meta_data) <- "exp_group" 
  
  # meta data colors
  ####color_for_later <- c("#ff6699","#92d050","#00b0f0",
  ####                     "#cc8128","#53c4b0","red")
  color_for_later <- color_samples
  for (i in 1:length(color_for_later)){
    if (isFALSE(str_detect(pattern = "\\#", string = color_for_later[i]))){
      rgb_vals <- col2rgb(color_for_later[i])
      hex_code <- rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
      color_for_later[i] <- hex_code
    } else {
      color_for_later[i] <- color_for_later[i]
    }
  }
  
  ##total_samples <- unique(meta_data$exp_group)
  total_samples <- unique(names(color_samples))
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
    if (isTRUE(more_than_a_group_per_sample)){
      ""
    }
    if (isFALSE(more_than_a_group_per_sample)){
      ## Parsing rule for the contrasts
      both_samples <- tt_names[j]
      samples1 <- stringr::str_split(both_samples,differentation_element, simplify = T)[1]
      samples2 <- stringr::str_split(both_samples,differentation_element, simplify = T)[2]
      if (samples2 == "rest") {
        samples2 <- unique(meta_data$exp_group[!meta_data$exp_group %in% samples1])
      } else {
          samples2 <- samples2}
      samples_to_work <- c(samples1,samples2)
      }
    
    ## meta data work
    meta_data_now <- meta_data %>% 
      filter(exp_group %in% c(samples_to_work))  
    meta_data_now <- meta_data_now %>% 
      mutate(Rows = rownames(meta_data_now)) %>% 
      arrange(factor(exp_group, levels = c(samples_to_work)))
    rownames(meta_data_now) <- meta_data_now$Rows 
    meta_data_now <- meta_data_now %>% 
      subset(select = -c(Rows))
    
    samples_now <- rownames(meta_data_now)[meta_data_now$exp_group %in% c(samples_to_work)]
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
    colnames(list_of_tts[[k]])[1] <- "Protein.Group"
    list_of_tts[[k]] <- list_of_tts[[k]][, !duplicated(colnames(list_of_tts[[k]]))]
    list_of_tts[[k]] <- list_of_tts[[k]] %>% 
      arrange(adj.P.Val)}
  tts <<- list_of_tts
  
  #### Time for the xlsx file
  wb <- createWorkbook("Proteomics Unit IJC")
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
    adjusted_p_val_columns <- which(colnames(dataset) == "adj.P.Val")
    adjusted_p_val_sign_rows <- which(dataset$adj.P.Val <= 0.05)
    added_value <- max(adjusted_p_val_sign_rows)+1
    adjusted_p_val_sign_rows <- c(adjusted_p_val_sign_rows,added_value)
    
    addStyle(wb, sheet = m, style = createStyle(halign = "right", fgFill = "#c3eccb", fontColour = "#1f7046"), 
             rows = adjusted_p_val_sign_rows, cols = adjusted_p_val_columns)
    
    
    ## Bold and right-center align all column names
    addStyle(wb, sheet = m, style = createStyle(textDecoration = "bold", halign = "left"), 
             rows = 1, cols = 1:ncol(dataset))
    
    ## Annotation style
    cols_of_annot <- colnames(annotation)
    annotation_col_indices <- sapply(cols_of_annot, function(col) which(names(dataset) == col))
    
    addStyle(wb, sheet = m, style = createStyle(border = c("bottom","right","left","top"), textDecoration = "bold"), 
             rows = min(annotation_col_indices), cols = 1:ncol(dataset))
    
    setColWidths(wb, sheet = m, cols = annotation_col_indices, widths = 20)
    
    ## ADD META DATA COLOR !!!!
    # Filter meta_data
    meta_data_now <- meta_data[rownames(meta_data) %in% colnames(dataset),]
    if (nrow(meta_data_now) == 0)
    {##remove(meta_data_now)
      next
    } else {
      for (group in 1:length(unique(meta_data_now$exp_group))){
        meta_data_now_2 <- meta_data_now
        group_now <- unique(meta_data_now_2$exp_group)[group]
        meta_data_now_2 <- meta_data_now_2 %>% 
          filter(exp_group == group_now)
        
        # what samples each colors
        columns_colour <- rownames(meta_data_now_2)[meta_data_now_2$exp_group == unique(meta_data_now_2$exp_group)]
        
        # do the which
        quin <- sapply(columns_colour, function(col) which(names(dataset) == col))
        
        # pick the colors
        colorss <- unique(meta_data_now_2$colorins[meta_data_now_2$exp_group == group_now])
        
        # put the style
        addStyle(wb, sheet = m, style = createStyle(halign = "right", fgFill = colorss,
                                                    border = c("bottom","right","left","top"), textDecoration = "bold"), 
                 rows = 1, cols = quin)
        
        
      }
      # what samples each colors
      # columns_colour_1 <- rownames(meta_data_now)[meta_data_now$exp_group == unique(meta_data_now$exp_group)[1]]
      # columns_colour_2 <- rownames(meta_data_now)[meta_data_now$exp_group == unique(meta_data_now$exp_group)[2]]
      
      # do the which
      # quin_1 <- sapply(columns_colour_1, function(col) which(names(dataset) == col))
      # quin_2 <- sapply(columns_colour_2, function(col) which(names(dataset) == col))
      
      # pick the colors
      # colors1 <- unique(meta_data_now$colorins[meta_data_now$exp_group == unique(meta_data_now$exp_group)[1]])
      # colors2 <- unique(meta_data_now$colorins[meta_data_now$exp_group == unique(meta_data_now$exp_group)[2]])
      
      # put the style
      # addStyle(wb, sheet = m, style = createStyle(halign = "right", fgFill = colors1,
      #                                            border = c("bottom","right","left","top"), textDecoration = "bold"), 
      #         rows = 1, cols = quin_1)
      
      #addStyle(wb, sheet = m, style = createStyle(halign = "right", fgFill = colors2,
      #                                            border = c("bottom","right","left","top"), textDecoration = "bold"), 
      #        rows = 1, cols = quin_2)
    }
    
    
    ## Widths for the rest of the data
    setColWidths(wb, sheet = m, cols = max(annotation_col_indices)+1:ncol(dataset), widths = 15)
  }
  saveWorkbook(wb = wb, filename)
  return(list_of_tts)
}
