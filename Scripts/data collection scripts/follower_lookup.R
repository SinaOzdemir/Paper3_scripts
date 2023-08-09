#follower look up and clean
#to be tested!

library(tidyverse)
library(rtweet)
dataDIR<-paste0(getwd(),"/Data")
followerDIR <- paste0(getwd(),"/Data/follower_ids")
saveDIR <- paste0(getwd(),"/Data/Clean_followers")

follower.sampler.token<-create_token(app = "follower_sampler",
                                     consumer_key = "8XdIXE4Ohb9E8ezo8U5ts1E8j",
                                     consumer_secret = "FK49c8YG7DqI5axCyUBdfCn0TrfKxQghqx9s84yjrlu7zHzOMo",
                                     access_token = "1151438784384421888-FVjuDOy97az8iBUuVOkZwp7Pyya3c4",
                                     access_secret = "VyJXEl8rxvKRFSW18uZdR2zpqcbW3NqTLOqZW3795wwwD",
                                     set_renv = F)


f.list<-list.files(path = followerDIR,pattern = "*.RDS",full.names = T)
eu.user.id<-gsub("C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/follower_ids/followers_|.RDS","",f.list)
meta.data<- tibble()


# putting it into a loop --------------------------------------------------

#need to add control sequences to accommodate rate limits
#need to add "else" part for the rich users
for (i in 27:length(f.list)) {
  eu.account<- readRDS(f.list[i])
  follower.vector<- as.character(eu.account$user_id)
  
  rl<-rate_limits(token = follower.sampler.token)
  
  if(any(rl$remaining <= 2)){
    
    message(paste0("Rate limit is reached at ", eu.user.id[i])," at ", Sys.time())
    
    Sys.sleep(15*60)
    
  }
  
  
  if(isTRUE(length(follower.vector)<90000)){
    
    
    
    follower_data<-lookup_users(users = follower.vector,parse = T,token = follower.sampler.token)
    
    follower_data_open<- dplyr::filter(follower_data, protected == F)
    
    saveRDS(object = follower_data_open,file = paste0(saveDIR,"/followers_", eu.user.id[i],".RDS"))
    
    eu.followers.meta<-tibble(eu_account = eu.user.id[i],
                              unique_follower_count = nrow(eu.account),
                              protected_user_count = sum(nrow(follower_data[which(follower_data$protected == T),])),
                              follower_sample_size = (nrow(follower_data)*.02))
    
    meta.data<-rbind(meta.data,eu.followers.meta)
    
    saveRDS(meta.data,file = paste0(dataDIR,"/eu_followers_meta.RDS"))
  } else {
  
    follower.vector.chunked<- split(follower.vector,ceiling(seq_along(follower.vector)/90000))    
    
    follower_data_bin<-tibble()
    
    for (j in 1:length(follower.vector.chunked)) {
      
      #control sequence
      
      rl<-rate_limits(token = follower.sampler.token)
      
      if(any(rl$remaining <= 2)){
        
        message(paste0("Rate limit is reached at rich user ", eu.user.id[i])," on  itteration ", j," at ", Sys.time())
        
        Sys.sleep(15*60)
        
      }
      
      
      #lookup
      
      ruser_follower_data <- lookup_users(follower.vector.chunked[[j]],parse = T,token = follower.sampler.token)
      
      ruser_follower_data_open<- dplyr::filter(ruser_follower_data, protected == F)
      
      follower_data_bin<-rbind(follower_data_bin,ruser_follower_data_open)
      
    }
    
    #save the follower info
    
    follower_data_bin_clean<- dplyr::filter(follower_data_bin, protected == F)
    
    saveRDS(follower_data_bin_clean, file = paste0(saveDIR,"/followers_",eu.user.id[i],".RDS"))
    #create the metadata and save it
    ruser_follower_meta <- tibble(eu_account = eu.user.id[i],
                                  unique_follower_count = nrow(eu.account),
                                  protected_user_count = nrow(follower_data_bin[which(follower_data_bin$protected == T),]),
                                  follower_sample_size = (nrow(follower_data_bin_clean)*.02))
    
    meta.data <- rbind(meta.data,ruser_follower_meta)
    saveRDS(meta.data,file = paste0(dataDIR,"/eu_followers_meta.RDS"))
    
  }
  
  message(paste0(eu.user.id[i]," is completed at ", Sys.time()))
}

