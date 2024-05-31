##########################
### make_all_contrasts ###
#########################

## desing = the design matrix 
## differentiating_element = the character element that differentiate the elements of the contrast
# ej. For a comparison named group1 vs. group3 differentiate element is " .vs " with the spaces !!!!!
#     For a comparison named group1_vs_group3 differentiate element is "_vs_"  wichout the spaces if no spaces are in the contrast matrix !!!

make_all_contrasts <- function(design, differentiating_element = " .vs "){
  group <- unique(as.character(colnames(design)))
  cb   <- combn(group, 2, FUN = function(x){paste0(x[1], "-", x[2])})
  contrasts<- limma::makeContrasts(contrasts=cb, levels=group)
  colnames(contrasts) <- gsub("-", differentiating_element, colnames(contrasts))
  return(contrasts)}
