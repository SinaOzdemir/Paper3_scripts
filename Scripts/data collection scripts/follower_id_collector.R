#follower sampler:
library(tidyverse)
library(rtweet)

dataDIR <- paste0(getwd(),"/Data/")

funcDIR<- paste0(getwd(),"/Scripts/functions/")

follower.sampler.token<-create_token(app = "follower_sampler",
                    consumer_key = "8XdIXE4Ohb9E8ezo8U5ts1E8j",
                    consumer_secret = "FK49c8YG7DqI5axCyUBdfCn0TrfKxQghqx9s84yjrlu7zHzOMo",
                    access_token = "1151438784384421888-FVjuDOy97az8iBUuVOkZwp7Pyya3c4",
                    access_secret = "VyJXEl8rxvKRFSW18uZdR2zpqcbW3NqTLOqZW3795wwwD",
                    set_renv = F)

eu.profs<-readRDS(file = paste0(dataDIR,"eu_profile.RDS")) %>%
  select(user_id) %>% as_vector(.x = .,.type = "character") %>%
  lookup_users(users = .,parse = T,token = follower.sampler.token) %>% users_data()

previous.id<-readRDS(file = paste0(dataDIR,"prof_to_scrape.RDS"))

#Scraping, looking up and sampling should be done at different stages so as to prevent data loss

for (i in 1:nrow(eu.profs)) {
  rl<- rate_limits(follower.sampler.token)
  
  if(isTRUE(any(rl$remaining <= 2))){
    message("rate limit is reached, sleeping 15 min")
    Sys.sleep(15*60)
  }
  
  follower_count<- as.integer(eu.profs[i,"followers_count"])
  user_id<- as.character(eu.profs[i,"user_id"])
  
  if(isTRUE(follower_count <= 75000L)){
    # I should put sampler here
    user.followers<-get_followers(user = user_id,n = 75000,parse = T,token = follower.sampler.token)
    follower_sample<- dplyr::filter(user.followers, !(user_id%in%previous.id))
    saveRDS(follower_sample,file = paste0(dataDIR,"follower_ids/","followers_",user_id,".RDS"))
    
  }else{
    # This does not sample, sampling should be done later.
    message("user has more than 75k followers, this may take a while")
    
    n_rep<- ifelse(is.integer(follower_count/75000),(follower_count/75000),ceiling(follower_count/75000))
    
    curs_val<-"-1"
    
    follower_container<-tibble()
    
    for (j in 1:n_rep) {
      
      rl_a<- rate_limits(token = follower.sampler.token)
      if(isTRUE(any(rl_a$remaining<=2))){
        message("rate limit is reached, waiting about 15 min")
        Sys.sleep(15*60)
      }
      
      test_scrape<-get_followers(user = user_id,n = 75000,retryonratelimit = F,page = curs_val,parse = T,token = follower.sampler.token)
      curs_val <- next_cursor(test_scrape)
      test_scrape_unique<- dplyr::filter(test_scrape, !(user_id%in%previous.id))
      follower_container<-rbind(follower_container,test_scrape_unique)
    }
    
    saveRDS(follower_container,file = paste0(dataDIR,"follower_ids/","followers_",user_id,".RDS"))
  }
}