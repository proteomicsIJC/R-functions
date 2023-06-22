maxquant_initalizer <- function(tmt,n_plex,maxquant_data){
  old_names <- colnames(maxquant_data)[grep(x = colnames(maxquant_data), pattern = "^Reporter intensity corrected")]
  reactives6 <- c("126","127n","128c","129n","130c","131")
  reactives10 <- c("126","127n","127c","128n","128c","129n","129c","130n","130c","131n")
  reactives11 <- c("126","127n","127c","128n","128c","129n","129c","130n","130c","131n","131c")
  reactives16 <- c("126","127n","127c","128n","128c","129n","129c","130n","130c","131n","131c","132n","132c","133n","133c","134n")
  if (tmt == "tmt6"){
    # tmt6
    reactives <- reactives6
  }
  
  if (tmt == "tmt10"){
    # tmt10
    reactives <- reactives10
  }
  
  if (tmt == "tmt11"){
    # tmt11
    reactives <- reactives11
  }
  
  if (tmt == "tmt16"){
    # tmt16
    reactives <- reactives16
  }
  
  
  number_of_plexes <- c(1:n_plex)
  plex <- c()
  for (i in 1:length(number_of_plexes)){
    plex[i] <- paste("plex",number_of_plexes[i],sep = "")
  }
  
  plex <- rep(plex, each = length(reactives))
  reactives <- rep(reactives, times = length(unique(plex)))
  
  new_names <- c()
  for (i in 1:length(plex)){
    new_names[i] <- paste("Intensity ",plex[i]," : ",toupper(tmt),"-",toupper(reactives[i]),sep = "")
  }
  
  name_changer <- data.frame(old = old_names,
                             new = new_names)
  
  setnames(maxquant_data, name_changer$old, name_changer$new)
  
  return(maxquant_data)
}
