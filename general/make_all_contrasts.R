make_all_contrasts <- function(design){
  group <- unique(as.character(colnames(design)))
  cb   <- combn(group, 2, FUN = function(x){paste0(x[1], "-", x[2])})
  contrasts<- limma::makeContrasts(contrasts=cb, levels=group)
  colnames(contrasts) <- gsub("-", "_vs_", colnames(contrasts))
  return(contrasts)}
