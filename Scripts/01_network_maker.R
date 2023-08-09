####################################################################
# Title: Follower Data cleaning and reshaping for network analysis #
# Author: Sina Özdemir                                             #
#         PhD candidate                                            #
#         Department of sociology and political science            #
#         sina.ozdemir@ntnu.no                                     #
####################################################################


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","here")

p_load(char = packs,install = T)

eu_account_ids<- readRDS(here("Data","eu_profile.RDS")) %>% pull(user_id)

feats<- c("user_id","created_at")

source(file = here("Scripts","functions","network_data_making_functions.R"),local = T)

save_path<- here("Data","L1_network_data")
# test --------------------------------------------------------------------

a<-readRDS(follower_ids_paths[1]) %>%
  select(user_id) %>% 
  mutate(eu_account = eu_account_ids[1]) %>% 
  mutate(follower = ifelse(user_id%in%scraped_follower_ids,1,0),
         replier = ifelse(user_id%in%scraped_replier_ids,1,0)) %>% 
  filter(follower == 1 | replier == 1)

write.table(x = a,file = here("Data","L1_network_data","network_test.csv"),append = T,quote = T,sep = ",",row.names = F,col.names = F,fileEncoding = "UTF-8")

# function test -----------------------------------------------------------


a<- readRDS(flw_frnds[1]) %>% discard(is.null)

function_test<- feat_extractor(dataset = a[[1]],features = feats,target_users = eu_account_ids,source_user = names(a)[1])



# loop implementation for followers ---------------------------------------
flw_frnds_dr<- list.files(path = here("Data","follower_friends"),pattern = "*.RDS|*.rds", full.names = T)
flw_network<- data.frame()
for (i in 1:length(flw_frnds_dr)) {
  
  cat(paste0(Sys.time(),": working on the batch ", i,"\n"))
  
  modus<- i%%100
  
  if(modus==0){
    #save every 10000 users or so and clean the memory so as not to have 
    #memory error
    cat(paste0("batch ", i, " is complete saving the data\n"))
    
    write_feather(x = flw_network,path = paste0(save_path,"/","L1_flw_network_",i-100,"-",i,".feather"))
    flw_network<-data.frame()
  }
  
  #read the user data batch
  user_list<- readRDS(file = flw_frnds_dr[i]) %>% 
    discard(is.null)
  
  #create a container for network data
  network_data<- data.frame()
  for (j in 1:length(user_list)){
    
    
    #extract the data from the list without unlisting
    user_name<- names(user_list)[j]
    friends_data<- user_list[[j]]
    
    #create the network data of user-eu for each user
    network_features<- feat_extractor(dataset = friends_data,
                                      source_user = user_name,
                                      features = feats,
                                      target_users = eu_account_ids)
    
    #append the user network data
    network_data<- rbind(network_data,network_features)
    
    #remove the individual user data 
    #so as not to overburden the ram
    remove(user_name,friends_data)
    

  }
  
  
  cat(paste0("finished the batch ",i,"\n"))
  
  #save the network data for the user batch
  flw_network<- rbind(flw_network,network_data)
  
  if(i==length(flw_frnds_dr)){
    cat(paste0("finished creating network for followers, saving the last batch\n"))
    
    write_feather(x = flw_network, path = paste0(save_path,"/","L1_flw_network_",i-100,"-",i,".feather"))
  }
    
}


# loop implementation for engagers ----------------------------------------

eng_frnds_dr<- list.files(path = here("Data","replier_friends"),pattern = "*.RDS|*.rds", full.names = T)
eng_network<- data.frame()
for (i in 1:length(eng_frnds_dr)) {
  
  cat(paste0(Sys.time(),": working on the batch ", i,"\n"))
  
  modus<- i%%100
  
  if(modus==0){
    #save every 10000 users or so and clean the memory so as not to have 
    #memory error
    cat(paste0("batch ", i, " is complete saving the data\n"))
    
    write_feather(x = eng_network,path = paste0(save_path,"/","L1_eng_network_",i-100,"-",i,".feather"))
    eng_network<-data.frame()
  }
  
  #read the user data batch
  user_list<- readRDS(file = eng_frnds_dr[i]) %>% 
    discard(is.null)
  
  #create a container for network data
  network_data<- data.frame()
  for (j in 1:length(user_list)){
    
    
    #extract the data from the list without unlisting
    user_name<- names(user_list)[j]
    friends_data<- user_list[[j]]
    
    #create the network data of user-eu for each user
    network_features<- feat_extractor(dataset = friends_data,
                                      source_user = user_name,
                                      features = feats,
                                      target_users = eu_account_ids)
    
    #append the user network data
    network_data<- rbind(network_data,network_features)
    
    #remove the individual user data 
    #so as not to overburden the ram
    remove(user_name,friends_data)
    
    
  }
  
  
  cat(paste0("finished the batch ",i,"\n"))
  
  #save the network data for the user batch
  eng_network<- rbind(eng_network,network_data)
  
  if(i==length(eng_frnds_dr)){
    cat(paste0("finished creating network for followers, saving the last batch\n"))
    
    write_feather(x = eng_network, path = paste0(save_path,"/","L1_eng_network_",i-100,"-",i,".feather"))
  }
  
}


# merge batched network datasets ------------------------------------------


flw_nt_dt_dr<- list.files(path = here("Data","L1_network_data"),pattern = "L1_flw_network",full.names = T)

map_dfr(flw_nt_dt_dr,.f = read_feather) %>% 
  write_feather(x = .,path = here("Data","L1_network_data","L1_flw_network_eu.feather"))

eng_nt_dt_dr<- list.files(path = here("Data","L1_network_data"),pattern = "L1_eng_network",full.names = T)

map_dfr(eng_nt_dt_dr,.f = read_feather) %>% 
  write_feather(x = .,path = here("Data","L1_network_data","L1_eng_network_eu.feather"))