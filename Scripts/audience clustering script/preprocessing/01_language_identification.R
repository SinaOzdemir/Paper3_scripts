# User embedding preprocessing step 1: Identifying languages


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","here","beepr")

p_load(char = packs)


follower_path<- here("Data","follower_tweets")

replier_path<- here("Data","replier_tweets")

follower_list<-list.files(path = follower_path, pattern = "*.RDS",full.names = T)

replier_list<-list.files(path = replier_path, pattern = "*.RDS",full.names = T)

user_list<- c(follower_list,replier_list)

# language_identification -------------------------------------------------

  
langs<- c()

for (i in 2531:length(user_list)) {
  cat("processing user group ",i,"\n")
  
  x<- readRDS(file = user_list[i])%>% 
    do.call("rbind",.) %>% 
    as.data.frame() 
  
  if(nrow(x) ==0){
    next
  } else{
    x<- x %>% 
    pull(lang) %>% 
    unique()
    
  langs<- c(langs,x)
  }
}

unique_langs<- unique(langs)
saveRDS(unique_langs, file = here("Data","audience_clustering_data","present_languages.rds"))

beepr::beep(sound = 4)