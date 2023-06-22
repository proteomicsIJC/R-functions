proteinGroupsCleanner <- function(ds = NULL) {
  
  message("Removing 'Only Identified by Site', 'Reverse' and 'Potential Contaminants' proteins...\n\n")
  if (is.null(ds)) {
    stop("Please, introduce a valid dataset!")
  }
  if (is.null(ds$only_identified_by_site)) {
    stop("The column 'Only Identified by Site' does not exist!")
  }
  if (is.null(ds$reverse)) {
    stop("The column 'Reverse' does not exist!")
  }
  if (is.null(ds$potential_contaminant)) {
    stop("The column 'Potential Contaminant' does not exist!")
  }
  if (all(is.na(ds$only_identified_by_site))) {
    ds$only_identified_by_site <- ""
  }
  if (all(is.na(ds$reverse))) {
    ds$reverse <- ""
  }
  if(all(is.na(ds$potential_contaminant))) {
    ds$potential_contaminant <- ""
  }
  s <- apply(ds[, c(grep("only_identified_by_site", colnames(ds)), grep("reverse", colnames(ds)), grep("potential_contaminant", colnames(ds)))]
             , 1, function(x) any(x == "+"))
  ds <- ds[!s, ]
  
  message("Removing remaining empty columns...\n\n")
  message("Creating 'dset.cleanned' object'...\n\n")
  dset.cleanned <- ds[, -c(grep("Only identified by site", colnames(ds)), grep("Reverse", colnames(ds)), grep("potential_contaminant", colnames(ds)))]
  message("DONE!\n\n")
  return(dset.cleanned)
}
