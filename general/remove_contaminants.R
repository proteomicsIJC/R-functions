#' Remove contaminants from the search
#' @param dataset a DIAN/Maxquant dataset with contaminants
#' @param contaminants path to a fasta file with contamiants
#' @param accession_name column_name where accessions of contaminants may be stored 
#' @export
remove_contaminants <- function(dataset,accession_name,
                                contaminants) {
  print(paste0("Removing contaminants from the data using ",contaminants))
  cn <- colnames(dataset)
  cont <- readLines(contaminants)
  cont <- cont[grep(">", cont)]
  cont <- substr(cont, 2, 7)
  cont <- cont[-which(cont %in% c("_",".","ENSEMB","REFSEQ","Strept","H-INV:"))]
  cleaned_dataset <- dataset[-c(unlist(lapply(cont, function(x) grep(x, dataset[,grep(accession_name, colnames(dataset))])))), ]
  return(cleaned_dataset)
}
