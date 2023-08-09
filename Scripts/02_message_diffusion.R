########################################################
#Title: Message diffusion capacity measurement         #
#Author: Sina Özdemir                                  #
#        PhD candidate                                 #
#        Department of Sociology and Political Science #
#        Norwegian University of Science and Technology#
#        sina.ozdemir@ntnu.no                          #
########################################################


# setup -------------------------------------------------------------------

require("pacman",character.only = T)

packs<- c("tidyverse","here","feather")

p_load(char = packs, install = T)

eu_accounts<- readRDS(file = here("Data","eu_profile.RDS")) %>%
  pull(user_id)

feats<- c("user_id","status_id","reply_to_user_id","quoted_user_id","retweet_user_id","followers_count")

save_dir<- here("Data","Diffusion_data")

source(file = here("Scripts","functions","engagement_rate_measurement.R"),local = T)

# diffusion via followers -------------------------------------------------

# flw_dr<- list.files(path = here("Data","follower_tweets"),pattern = "*.RDS|*.rds",full.names = T)
# 
# 
# map_dfr(.x = flw_dr,diffusion_data_extr,features = feats,target_users = eu_accounts) %>% 
#   feather::write_feather(x = .,path = here("Diffusion_data","L1_flw_diffusion.feather"))
#   

# diffusion via engagers --------------------------------------------------
# 
# eng_dr<- list.files(path = here("Data","replier_tweets"),pattern = "*.RDS|*.rds",full.names = T)
# 
# map_dfr(.x = eng_dr,diffusion_data_extr,features = feats,target_users = eu_accounts) %>% 
#   feather::write_feather(x = .,path = here("Diffusion_data","L1_eng_diffusion.feather"))


#Error: cannot allocate vector of size 32.0 Mb
#Error: cannot allocate vector of size 652.3 Mb



# for loop implementation -------------------------------------------------



##Purrr implementation is very RAM intensive, switching to for loop implementation with batching
flw_dr<- list.files(path = here("Data","follower_tweets"),pattern = "*.RDS|*.rds",full.names = T)

flw_eng<- data.frame()

for (i in 1:length(flw_dr)) {
  
  user_list<- readRDS(file = flw_dr[i]) %>% 
    discard(is.null)
  
  modus<- i%%100
  
  if(modus == 0){
    cat(paste0("writing batch",i-100," and ", i))
    
    write_feather(x = flw_eng,path = paste0(save_dir,"/","L1_flw_eng_",i-100,"_",i,".feather"))
    flw_eng<- data.frame()
  }
  
  flw_df<- data_frame()
  for (j in 1:length(user_list)) {
    indv_eng<- ind_dif_rate(dataset = user_list[[j]],features = feats,target_users = eu_accounts)
    
    flw_df<- rbind(flw_df,indv_eng)
  }
  
  flw_eng<- rbind(flw_eng,flw_df)
  
  if(i == length(flw_dr)){
    cat("Saving the last batch")
    write_feather(x = flw_eng,path = paste0(save_dir,"/","L1_flw_eng_",i-100,"_",i,".feather"))
  }
}



# eng diffusion -----------------------------------------------------------



rep_dr<- list.files(path = here("Data","replier_tweets"),pattern = "*.RDS|*.rds",full.names = T)

rep_eng<- data.frame()

for (i in 1:length(rep_dr)) {
  
  user_list<- readRDS(file = rep_dr[i]) %>% 
    discard(is.null)
  
  modus<- i%%100
  
  if(modus == 0){
    cat(paste0("writing batch",i-100," and ", i))
    
    write_feather(x = rep_eng,path = paste0(save_dir,"/","L1_rep_eng_",i-100,"_",i,".feather"))
    rep_eng<- data.frame()
  }
  
  rep_df<- data_frame()
  for (j in 1:length(user_list)) {
    indv_eng<- ind_dif_rate(dataset = user_list[[j]],features = feats,target_users = eu_accounts)
    
    rep_df<- rbind(rep_df,indv_eng)
  }
  
  rep_eng<- rbind(rep_eng,rep_df)
  
  if(i == length(rep_dr)){
    cat("Saving the last batch")
    write_feather(x = rep_eng,path = paste0(save_dir,"/","L1_rep_eng_",i-100,"_",i,".feather"))
  }
}

