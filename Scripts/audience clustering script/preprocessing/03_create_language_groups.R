# User embeddings preprocessing step 3: re-assign users by language groups


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","here","feather")

p_load(char = packs)

langs<- readRDS(file = here("Data","audience_clustering_data","present_languages.rds"))

language_groups<- readRDS(file = here("Data","audience_clustering_data","language_groups.RDS")) %>% 
  select(-dominant_lang)


follower_path<- here("Data","follower_tweets")

replier_path<- here("Data","replier_tweets")

follower_list<-list.files(path = follower_path, pattern = "*.RDS",full.names = T)

replier_list<-list.files(path = replier_path, pattern = "*.RDS",full.names = T)

user_list<- c(follower_list,replier_list)

remove(follower_list,follower_path,replier_list,replier_path,packs)

save_dir<- "D:/paper3_audience_clustering_data"
# re-organize users into language groups ----------------------------------

for(i in 2: length(langs)){
  
  language = langs[i]
  cat("processing users for language: ",language,"\n")
  
  lang = list()
  
  language_users<- language_groups %>% 
    filter(lang == language) %>% 
    pull(user_id)
  
  for (j in 1:length(user_list)) {
    
    cat("processing user group",j,"\n")
    
    x<- readRDS(user_list[j]) %>%
      do.call("rbind",.)
    
    if(nrow(x)==0){
      cat("no user data, moving on")
      next
    }else{
    cat("trimming user data\n")
    x<-x%>%
      filter(user_id%in%language_users) %>% 
      select(user_id,status_id,text,lang) %>% 
      split(x = .,f = .$user_id)
    cat("appending user data to list \n")
    
    lang <- append(lang,x)
    remove(x)
    gc(verbose = F,full = T)
    }
    if((j%%100) == 0){
      cat("saving the list and creating a new one...\n")
    saveRDS(lang,file = paste0(save_dir,"/",language,"_",j,"_users.RDS"))
    lang = list()
    }
   
  }
  cat("saving the full list...\n")
  saveRDS(lang,file =  paste0(save_dir,"/",language,"_",j,"_users.RDS"))
}

beepr::beep(sound = 4)

#left at i = 15, j = 2602.