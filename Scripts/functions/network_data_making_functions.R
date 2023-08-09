library(pacman)

packs<- c("tidyverse","here","feather")

p_load(char = packs, install = T)

feat_extractor<- function(dataset,source_user,features,target_users){
  
  c_names<- colnames(dataset)
  
  if (length(c_names)>0){
    feat_exist<- all(features%in%c_names)
    #make sure that all variables of interest are of the same type/class so 
    #that there is no problem later on merging
   
    
    #make sure that variables of interest are present in the dataset
    if(isTRUE(feat_exist)){
      dataet<- dataset %>% drop_na(created_at)
      
      dataset<- dataset %>% 
        mutate(user_id = as.character(user_id))
      #I don't need created_at variable in the final dataset.
      #only the following patter of user->eu
      extracted_feats<- dataset %>%
        select(user_id) %>% 
        filter(user_id%in%target_users) %>% 
        rename(eu_accounts = user_id) %>% 
        mutate(user_id = source_user)
      
      return(extracted_feats)
      
      
    }else{
      
      #if variables of interest are not present, return null.
      #Returning null does not break rbind routine of mapdfr
      cat("Specified features don't exist in the dataset, retuning null\n")
      return(NULL)
      
    }
    
    }else{
      
      cat("Dataset columns are missing, returning null\n")
      return(NULL)
    }
  
}


network_maker<- function(file_path){
  print(paste("working on ", file_path))
  
  user_list<- readRDS(file_path) %>%
    discard(is.null)
    
  
  user_names<- names(user_list)
  
  network_chunk<-map2_dfr(.x = user_list,.y = user_names,.f = feat_extractor,target_users = eu_accounts,features= c("user_id","created_at"))
  
  return(network_chunk)
}


