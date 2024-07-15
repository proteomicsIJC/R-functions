#' Change the direction of contrasts in a limma contrast matrix
#' @param contmat  a contrast matrix
#' @param comparisons_to_change  a character vector with all the comparisons which order shoud be changed
#' @param differentiating_element  the character element that differentiate the elements of the contrast
#' ej. For a comparison named group1 vs. group3 differentiate element is " .vs " with the spaces !!!!!
#'     For a comparison named group1_vs_group3 differentiate element is "_vs_"  wichout the spaces if no spaces are in the contrast matrix !!!
#' @export
upsidedown <- function(contmat, comparisons_to_change=NULL, 
                       differentiating_element = " .vs "){
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
        before <- str_split(colnames(contmat)[j], differentiating_element , simplify = T)[1]
        after <- str_split(colnames(contmat)[j], differentiating_element , simplify = T)[2]
        colnames(contmat)[j] <- paste0(after, differentiating_element ,before,collapse = "")
      }
    }
  }
  return(contmat)
}
