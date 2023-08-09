#rich user (F_k => 75000) follower sampler 
library(rtweet)
library(tidyverse)
dataDIR<- paste0(getwd(),"/Data/follower_ids/")

f.list<- list.files(path = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/follower_ids",pattern = "*.RDS",full.names = T)

follower.token<-create_token(app = "follower_sampler",
                             consumer_key = "8XdIXE4Ohb9E8ezo8U5ts1E8j",
                             consumer_secret = "FK49c8YG7DqI5axCyUBdfCn0TrfKxQghqx9s84yjrlu7zHzOMo",
                             access_token = "1151438784384421888-FVjuDOy97az8iBUuVOkZwp7Pyya3c4",
                             access_secret = "VyJXEl8rxvKRFSW18uZdR2zpqcbW3NqTLOqZW3795wwwD",
                             set_renv = F)

for (i in 1:length(f.list)) {
  
  user.followers<-readRDS(f.list[i])
  
  if(ncol(user.followers)==1){
    #chunk the follower ids to accommodate rate limits
    chunked.profs<- split(user.followers,ceiling(seq_along(user.followers)/90000))
    #get the EU account id from the file name
    eu.id<-gsub(pattern = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/follower_ids/followers_|.RDS",replacement = "",x = f.list[i])
    #create a bin for follower profiles
    user.prof.bin<-tibble()
    for (j in 1:length(chunked.profs)) {
      #rate limit controller here
      rl<-rate_limits(token = follower.token)
      
      if(any(rl$remaining<=2)){
        Sys.sleep(15*60)
      }
      #look up profiles here
      prof.batch<- unlist(chunked.profs[j])
      f.profs<- lookup_users(users = prof.batch,parse = T,token = follower.token)
      #bin them in a tibble here
      user.prof.bin<-rbind(user.prof.bin,f.profs)
      
    }
    # filter them here
    open.profs<- filter(user.prof.bin, protected == F)
    
    # sample them here
    sample.profs<-slice_sample(open.profs, prop = .02)
    
    #save it here
    
    saveRDS(object = sample.profs,file = paste0(dataDIR,"followers_",eu.id,".RDS"))
  }else{
    remove(user.followers)
  }
  
}
