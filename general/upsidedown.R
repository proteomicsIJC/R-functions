upsidedown <- function(contmat, comparisons_to_change=NULL){
  if (is.null(comparisons_to_change)){
    contmat <- contmat}
  if (!is.null(comparisons_to_change)){
    for (i in 1:length(contmat)){
      if (colnames(contmat)[i] %in% comparisons_to_change){
        contmat[,i] <- (contmat[,i] * (-1))
      }
    }
    for (j in 1:length(colnames(contmat))){
      if (colnames(contmat)[j] %in%  comparisons_to_change){
        before <- str_split(colnames(contmat)[j], "_vs_", simplify = T)[1]
        after <- str_split(colnames(contmat)[j], "_vs_", simplify = T)[2]
        colnames(contmat)[j] <- paste0(after,"_vs_",before,collapse = "")
      }
    }
  }
  return(contmat)
}
