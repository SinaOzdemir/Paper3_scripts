replier_friends_dir<- "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/replier_friends"

replier_followers_dir<- "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/replier_followers"


# L3 of repliers ----------------------------------------------------------


replier_friends<- list.files(path = replier_friends_dir,pattern = "*.RDS",full.names = T)

replier_friends_information<-data.frame()

for(i in 1:length(replier_friends)){
  replier_group<- readRDS(file = replier_friends[i])
  replier_names<- c()
  replier_friends_count<- c()
  for(j in 1:length(replier_group)){
    replier_names[[j]]<-names(replier_group[j])
    replier_friends_count[[j]]<- nrow(replier_group[[j]])
    
    
  }
  replier_info<- as.data.frame(cbind(replier_names,replier_friends_count))
  replier_friends_information<- rbind(replier_friends_information,replier_info)
}



replier_followers<- list.files(path = replier_followers_dir,pattern = "*.RDS",full.names = T)

replier_followers_information<-data.frame()

for(i in 1:length(replier_followers)){
  replier_group<- readRDS(file = replier_followers[i])
  replier_names<- c()
  replier_followers_count<- c()
  for(j in 1:length(replier_group)){
    replier_names[[j]]<-names(replier_group[j])
    replier_followers_count[[j]]<- nrow(replier_group[[j]])
    
    
  }
  replier_info<- as.data.frame(cbind(replier_names,replier_followers_count))
  replier_followers_information<- rbind(replier_followers_information,replier_info)
}

replier_information<- inner_join(replier_friends_information,replier_followers_information,by = "replier_names")

a<-replier_information %>% 
  mutate(replier_names = as.character(replier_names))%>%
  drop_na() %>% 
  mutate(replier_followers_count = as.vector(replier_followers_count,mode = "numeric"),
         replier_friends_count = as.vector(replier_friends_count, mode = "numeric"))
  
b<- sum(a$replier_friends_count+a$replier_followers_count)

saveRDS(object = a,file = here("Data","L2_network","replier_L2_network_size.RDS"))
# L3 of followers ---------------------------------------------------------

follower_friends_dir<- "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/follower_friends"

follower_followers_dir<- "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/follower_followers"


follower_friends<- list.files(path = follower_friends_dir,pattern = "*.RDS",full.names = T)

follower_friends_information<-data.frame()

for(i in 1:length(follower_friends)){
  follower_group<- readRDS(file = follower_friends[i])
  follower_names<- c()
  follower_friends_count<- c()
  for(j in 1:length(follower_group)){
    follower_names_j<-names(follower_group[j])
    follower_names<- c(follower_names,follower_names_j)
    follower_friends_count_j<- nrow(follower_group[[j]])
    follower_friends_count<- c(follower_friends_count,follower_friends_count_j)
    
  }
  follower_info<- as.data.frame(cbind(follower_names,follower_friends_count))
  follower_friends_information<- rbind(follower_friends_information,follower_info)
}



follower_follower<- list.files(path = follower_followers_dir,pattern = "*.RDS",full.names = T)

follower_follower_information<-data.frame()

for(i in 1:length(follower_follower)){
  follower_group<- readRDS(file = follower_follower[i])
  follower_names<- c()
  follower_follower_count<- c()
  for(j in 1:length(follower_group)){
    follower_names_j<-names(follower_group[j])
    follower_names<- c(follower_names,follower_names_j)
    follower_follower_count_j<- nrow(follower_group[[j]])
    follower_follower_count<- c(follower_follower_count,follower_follower_count_j)
    
    
  }
  follower_info<- as.data.frame(cbind(follower_names,follower_follower_count))
  follower_follower_information<- rbind(follower_follower_information,follower_info)
}


follower_friends_information<- follower_friends_information %>% 
  mutate(follower_names = as.character(follower_names)) %>% 
  drop_na() %>% 
  mutate(follower_friends_count = as.numeric(follower_friends_count))

follower_follower_information<- follower_follower_information %>% 
  mutate(follower_names = as.character(follower_names)) %>% 
  drop_na() %>% 
  mutate(follower_follower_count = as.numeric(follower_follower_count))
  

L3_follower_info<- inner_join(follower_friends_information,follower_follower_information, by = "follower_names")

saveRDS(object = L3_follower_info,file = here("Data","L2_network","follower_L2_network_size.RDS"))
