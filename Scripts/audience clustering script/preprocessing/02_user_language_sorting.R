# User embedding preprocessing step 2: sort users by language groups


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","here","beepr")

p_load(char = packs)
follower_path<- here("Data","follower_tweets")

replier_path<- here("Data","replier_tweets")

follower_list<-list.files(path = follower_path, pattern = "*.RDS",full.names = T)

replier_list<-list.files(path = replier_path, pattern = "*.RDS",full.names = T)

user_list<- c(follower_list,replier_list)

# sort users --------------------------------------------------------------

language_groups = data.frame()

for (i in 1:length(user_list)){
  cat("processing user ",i,"\n")
  
  x<- readRDS(file = user_list[i]) %>% 
    do.call("rbind",.)
  
  if(nrow(x) == 0){
    next
  }else{
    x<- x %>% select(user_id,lang) %>% 
    group_by(user_id,lang) %>% 
    tally() %>% 
    group_by(user_id) %>% 
    filter(n == max(n)) %>% 
    ungroup() %>% 
    rename(dominant_lang = n)
  
  language_groups<- rbind(language_groups, x)
  
  }
}

saveRDS(language_groups,file = here("Data","audience_clustering_data","language_groups.RDS"))
beepr::beep(sound = 4)