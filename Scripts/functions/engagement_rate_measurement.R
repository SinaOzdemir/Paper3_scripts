########################################################
#Title: Message diffusion capacity functions           #
#Author: Sina Özdemir                                  #
#        PhD candidate                                 #
#        Department of Sociology and Political Science #
#        Norwegian University of Science and Technology#
#        sina.ozdemir@ntnu.no                          #
########################################################


# setup -------------------------------------------------------------------

require("pacman",character.only = T)

packs<- c("tidyverse","here")

p_load(char = packs, install = T)


options(dplyr.summarise.inform = FALSE)

eu_accounts<- readRDS(file = here("Data","eu_profile.RDS")) %>%
  pull(user_id)

##TODO:
#1) Test functions +
#2) ind_dif_rate function return too many NULLs, double check that it is due to data not the function
# functions ---------------------------------------------------------------

ind_dif_rate<- function(dataset,features, target_users){
  
  c_names<- colnames(dataset)
  
  
  feats_exist<- all(features%in%c_names)
  
  
  if(isTRUE(feats_exist)){
    follower_id<- dataset %>% 
      pull(user_id) %>% 
      unique()
    
    user_info<- dataset %>% 
      select(user_id,followers_count) %>% 
      filter(!duplicated(user_id))
    
    dataset<- dataset %>% 
      mutate(across(contains(match = "id"),~as.character(.x))) %>% 
      mutate(followers_count = as.numeric(followers_count))
    
    reply_n<- dataset %>%
      rename(engaged_user = reply_to_user_id) %>% 
      group_by(user_id, engaged_user) %>% 
      summarise(reply_count= n()) %>% 
      pivot_longer(reply_count,names_to = "engagement_type",values_to = "engagement_rate") %>% 
      drop_na(engagement_type)
    
    retweet_n<- dataset %>%
      rename(engaged_user = retweet_user_id) %>% 
      group_by(user_id,engaged_user) %>% 
      summarise(retweet_count = n()) %>% 
      pivot_longer(retweet_count,names_to = "engagement_type",values_to = "engagement_rate")%>% 
      drop_na(engagement_type)
    
    
    quote_n<- dataset %>%
      rename(engaged_user = quoted_user_id) %>% 
      group_by(user_id,engaged_user) %>% 
      summarise(quote_count = n())%>% 
      pivot_longer(quote_count,names_to = "engagement_type",values_to = "engagement_rate")
    
    
    engagement_data<- rbind(reply_n,retweet_n,quote_n) %>%
      drop_na(engaged_user)
    
    engagement_eu<- engagement_data %>%
      filter(engaged_user %in%eu_accounts)
    
    if(nrow(engagement_eu)==0){
      
      cat("The User didn't engage with EU executives, returning 0 values!\n")
      
      # This operation artificially excacerbates RAM use by generating 0 data.
      # It would be best if I return NULL for non-engagers for now but add in 0 after
      
      engagement_type = rep(c("reply_count","retweet_count","quote_count"),times = length(target_users))
      engaged_user = rep(target_users,each = 3)
      user_id = rep(follower_id, rep = length(engagement_type))
      engagement_rate = rep.int(0,times = length(engagement_type))
      #cbind modifies variable types to the most common
      engagement_eu<- data.frame(engagement_type = engagement_type,
                                 engaged_user = engaged_user,
                                 user_id = user_id,
                                 engagement_rate = engagement_rate)%>% 
        left_join(.,user_info, by = "user_id") %>%
        mutate(diffusion_rate = (engagement_rate*followers_count))
      
      return(engagement_eu)
      
      return(NULL)
    }else{
      cat("The user engaged with the EU, returning relevant values!\n")
      
      engagement_eu<- engagement_eu %>% 
        left_join(.,user_info,by = "user_id") %>% 
        mutate(diffusion_rate = (engagement_rate*followers_count))
      
      return(engagement_eu)
    } }else{
      
      cat("Required features don't exist in the dataset, returning NULL\n")
      
      return(NULL)
    }
}


diffusion_data_extr<- function(file_path,features,target_users){
  
  cat(paste0("working on ", file_path))
  
  
  user_list<- readRDS(file_path) %>% 
    discard(is_null)
  
  diffusion_data<- map_dfr(.x = user_list,.f = ind_dif_rate,features = features, target_users = target_users)
  
  return(diffusion_data)
  
}
