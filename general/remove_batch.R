####################
### remove_batch ###
####################

## dataset = a long format dataset with a column named normalized_intensity, protein_group and sample_or_pool,
# a column named plex is required when using the POOL.
## remove = yes or no
## use_combat = T/F
## use_pool = T/F use the pool to remove the effect: to do the operation we will need a column named sample_or_pool indicating wether if a sample is sample or POOL
# samples will be annotated as sample and POOLs as POOL. "IMPORTANT" another column with the plexs of the experiment as this has been thought as a workarround 
# for TMT.
## use_remove_batch_effect = T/F use remove_batch_effect function
## where_is_batch_1 = named vector with the batches of batch effector 1, used in use_combat and in use_remove_batch_effect
## where_is_batch_2 = named vector with the batches of batch effector 2, used only in use_remove_batch_effect
## report_results = make it or not able to write in the used_parameters.txt file

remove_batch <- function(dataset, remove,use_combat = F, use_pool =F, use_remove_batch_effect = F ,where_is_the_batch1 = NULL, where_is_the_batch2 = NULL,
                         report_results = T){
  if (remove == "no"){
    un_batched_data <- dataset
    
    un_batched_data$unbatched_intensity <- un_batched_data$normalized_intensity
    
    
    batching <- dataset
    batching <- reshape2::dcast(batching, 
                      protein_group ~ sample_name, value.var="normalized_intensity", fun.aggregate = median)
    rownames(batching) <- batching$protein_group
    batching <- batching[,-1]
    batching <<- batching
    print("Batch effect won't be removed")
    if (report_results){
    cat("remove_batch",file = "./results/used_parameters.txt",sep = "\n", append = T)
    cat(paste0("remove == no"), file = "./results/used_parameters.txt", sep = "\n", append = T)
    cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")}
  }
  if (remove == "yes" & use_pool == T){
    print("Batch effect removed using pool data")
    cols <- c("plex","protein_group","tmt")
    dataset <- dataset
    median_pooled <- median(dataset$normalized_intensity)
    batching <- dataset %>%
      reshape2::dcast(plex + protein_group + sample_name ~ sample_or_pool, value.var = "normalized_intensity", fun.aggregate = median) %>% 
      fill(POOL, .direction = c("up")) %>%
      mutate(unbatched_intensity = sample - POOL) %>% 
      mutate(was_pool = ifelse(is.na(sample),"Was pool","Was not pool")) %>% 
      filter(was_pool == "Was not pool") %>% 
      mutate(unbatched_intensity = unbatched_intensity + median_pooled) %>%
      relocate(sample_name, protein_group,plex, unbatched_intensity)
    
    un_batched_data <- merge(batching, dataset, by = c("sample_name","protein_group","plex"))
    
    remove_this_columns <- c("POOL","sample","was_pool","sample_number","sample_or_pool")
    
    for (i in 1:length(remove_this_columns)){
      un_batched_data <- select(un_batched_data, subset = -c(remove_this_columns[[i]]))
    }
    batching <- batching
    batching <- reshape2::dcast(batching, 
                      protein_group ~ sample_name, value.var="unbatched_intensity", fun.aggregate = median)
    rownames(batching) <- batching$protein_group
    batching <- batching[,-1]
    batching <<- batching
    
    if (report_results){
    cat("remove_batch",file = "./results/used_parameters.txt",sep = "\n", append = T)
    cat(paste0("remove == yes ","use_pool == T"), file = "./results/used_parameters.txt", sep = "\n", append = T)
    cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")}
  }
  if (remove == "yes" & use_combat == T){
    print("Removing batch effect usign ComBat")
    batching <- dataset %>% 
      filter(sample_or_pool == "sample")
    
    batching <- reshape2::dcast(batching, 
                      protein_group ~ sample_name, value.var="normalized_intensity", fun.aggregate = median)
    
    rownames(batching) <- batching$protein_group
    batching <- batching[,-1]
    batching <- ComBat(dat = batching, batch = where_is_the_batch1)
    
    samples <- rep(colnames(batching), each = length(unique(rownames(batching))))
    pgs <- rep(rownames(batching), times = length(unique(samples)))
    values <- unlist(as.vector(batching), use.names = F)
    
    batched_data_long <- data_frame("sample_name" = samples,
                                    "protein_group" = pgs,
                                    "unbatched_intensity" = values)
    
    un_batched_data <- merge(dataset, batched_data_long)
    un_batched_data <- un_batched_data %>%
      relocate(unbatched_intensity, .after = normalized_intensity)
    batching <<- batching
    if (report_results){
    cat("remove_batch",file = "./results/used_parameters.txt",sep = "\n", append = T)
    cat(paste0("remove == yes "," use_combat == T"), file = "./results/used_parameters.txt", sep = "\n", append = T)
    cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")}
  }
  if (remove == "yes" & use_remove_batch_effect == T){
    print("Removing batch effect usign removeBatchEffect")
    batching <- dataset %>% 
      filter(sample_or_pool == "sample")
    
    batching <- reshape2::dcast(batching, 
                      protein_group ~ sample_name, value.var="normalized_intensity", fun.aggregate = median)
    
    rownames(batching) <- batching$protein_group
    batching <- batching[,-1]
    if (is.null(where_is_the_batch2)){
      batching <- removeBatchEffect(x = batching, batch = where_is_the_batch1)
    }
    if (!is.null(where_is_the_batch2)){
      batching <- removeBatchEffect(x = batching, batch = where_is_the_batch1, batch2 = where_is_the_batch2)
    }
    samples <- rep(colnames(batching), each = length(unique(rownames(batching))))
    pgs <- rep(rownames(batching), times = length(unique(samples)))
    values <- unlist(as.vector(batching), use.names = F)
    
    batched_data_long <- data_frame("sample_name" = samples,
                                    "protein_group" = pgs,
                                    "unbatched_intensity" = values)
    
    un_batched_data <- merge(dataset, batched_data_long)
    un_batched_data <- un_batched_data %>%
      relocate(unbatched_intensity, .after = normalized_intensity)
    batching <<- batching
    if (report_results){
    cat("remove_batch",file = "./results/used_parameters.txt",sep = "\n", append = T)
    cat(paste0("remove == yes "," use_remove_batch_effect == T"), file = "./results/used_parameters.txt",  append = T, sep = "\n")
    cat(paste0(rep("_",50), collapse = ""), file = "./results/used_parameters.txt",append = T, sep = "\n")}
    
  }
  return(un_batched_data)
}
