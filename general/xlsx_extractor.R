xlsx_tt <- function(tt_cleaned_list){
  wb <- createWorkbook("Ignasi Jarne Sanz")
  names_for_wb <- names(tt_cleaned_list)
  for (i in 1:length(tt_cleaned_list)){
    addWorksheet(wb, names_for_wb[i])
    writeData(wb, sheet = i, tt_cleaned_list[[i]])
    
    # Create general style
    header_style_global <- createStyle(fontColour = "black",halign = "right",
                                       border = c("top","left","right","bottom"), textDecoration = "bold")
    style_for_not_head <- createStyle(fontColour = "black", halign = "right", valign = "center",
                                      border = c("top","left","right","bottom"))
    
    # Add general styles
    addStyle(wb, sheet = i, header_style_global, rows = 1, cols = 1:ncol(tt_cleaned_list[[i]]), gridExpand = T)
    addStyle(wb, sheet = i, style_for_not_head, rows = 2:nrow(tt_cleaned_list[[i]]), cols = 1:ncol(tt_cleaned_list[[i]]), gridExpand = T)
    
    
    # Possible headers
    samp1 <- str_split(names_for_wb[i], "_vs_", simplify = T)[1]
    samp2 <- str_split(names_for_wb[i], "_vs_", simplify = T)[2]
    
    # Save ncols of possible headers
    samp1 <- grep(samp1, colnames(tt_cleaned_list[[i]]))
    samp2 <- grep(samp2, colnames(tt_cleaned_list[[i]]))
    
    # Create special styles
    header_style_samp1 <- createStyle(fontColour = "black", halign = "center", 
                                      border = c("top","left","right","bottom"), fgFill = "#03D3FF", textDecoration = "bold")
    header_style_samp2 <- createStyle(fontColour = "black", halign = "center", 
                                      border = c("top","left","right","bottom"), fgFill = "#00B159", textDecoration = "bold")
    
    # Add special styles
    addStyle(wb, sheet = i, header_style_samp1, rows = 1, cols = samp1, gridExpand = T)
    addStyle(wb, sheet = i, header_style_samp2, rows = 1, cols = samp2, gridExpand = T)
    
    # Freezee
    freezePane(wb, sheet = i, firstActiveCol = 2, firstActiveRow = 2)
  }
  saveWorkbook(wb, "./results/final_TT.xlsx", overwrite = T)
  print("Xlsx has been written corretcly")
}
