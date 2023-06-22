REMOVALcontaminantsPEAKS <- function(dataset, contaminants, accession_name) {
  print(paste0("Removing contaminants from the data using ",contaminants))
  cn <- colnames(dataset)
  cont <- readLines(contaminants)
  cont <- cont[grep(">", cont)]
  cont <- substr(cont, 2, 7)
  cont <- cont[-which(cont %in% c("_",".","ENSEMB","REFSEQ","Strept","H-INV:"))]
  cleaned_dataset <- dataset[-c(unlist(lapply(cont, function(x) grep(x, dataset[,grep(accession_name, colnames(dataset))])))), ]
  return(cleaned_dataset)
}
