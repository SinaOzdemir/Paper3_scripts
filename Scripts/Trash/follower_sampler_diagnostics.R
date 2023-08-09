#follower sampling diagnosis

#Problem definition: some of the sampled user ID files are still 1kb,
#meaning that they are empty

#Tasks: 
#1) identify which accounts' followers are not sampled correctly----
workdir<-paste0(dataDIR,"follower_ids/")

follower.ids<-list.files(path = workdir,pattern = "*.RDS",full.names = T)

account.ids<-gsub("C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/follower_ids/followers_|.RDS","",follower.ids)

eu.profs<-readRDS(file = paste0(dataDIR,"eu_profile.RDS"))

eu.profs$sampled<- ifelse(eu.profs$user_id%in%account.ids,1,0)

unsampled.accounts<-eu.profs[which(eu.profs$sampled==0),]
## extra problem: ecb and philhogans accounts are not yet sampled



null_reader<-function(d.source){
  
  data<- readRDS(d.source)
  if(is.null(data)){
    problem.user<- gsub("C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/follower_ids/followers_|.RDS","",d.source)
    return(problem.user)
  }else{
    remove(data)
  }
}

pr.users<-lapply(follower.ids,FUN = null_reader)
pr.users<-pr.users[!sapply(pr.users, is.null)]

pr.users.vec<-unlist(x = pr.users)

eu.profs$pr.users<-ifelse(eu.profs$user_id%in%pr.users.vec,1,0)

pr.users.dta<-eu.profs[which(eu.profs$pr.users==1),]

barplot(height = as.numeric(pr.users.dta$followers_count),
        main = "missing accounts",
        ylab = "follower counts")
#so all the problem accounts are those who have less than 75000 so, 
#what might causing the problem?

#2) identify the point of origin of the problem.


follower_sampler<- function(user.id,follower.count,token,samp.prop,previous.ids){
  
  
  follower_sample_size<- round(follower.count*samp.prop)
  
  message(paste0("starting with the followers of ", user.id," it has ", follower.count," followers"))
  tryCatch(expr = {
    
    message("user has less than 75000 followers; starting to sample them")
    #look up the follower ids
    user_followers<-rtweet::get_followers(user = user.id,n = 75000,retryonratelimit = F,parse = T,verbose = F,token = token)
    #convert the follower ids to vector
    user_followers.vec<- as_vector(user_followers,"character")
    #remove users that are already sampled in replier and retweeter sampling process
    user_followers.uniq<- user_followers.vec[-which(user_followers.vec%in%previous.ids)]
    #look up hydrated info of followers
    #most likely fucks?***********
    e.message<-paste(Sys.time(),user_id, err, sep = "\n")
    write(x = e.message,file = paste0(dataDIR,"follower_sample_errors.txt"),append = T)
  }
  )
}

#3) identify a sustainable solution to the problem

#do the collection in one loop look up users later
follower.sampler.token<-create_token(app = "follower_sampler",
                                     consumer_key = "8XdIXE4Ohb9E8ezo8U5ts1E8j",
                                     consumer_secret = "FK49c8YG7DqI5axCyUBdfCn0TrfKxQghqx9s84yjrlu7zHzOMo",
                                     access_token = "1151438784384421888-FVjuDOy97az8iBUuVOkZwp7Pyya3c4",
                                     access_secret = "VyJXEl8rxvKRFSW18uZdR2zpqcbW3NqTLOqZW3795wwwD",
                                     set_renv = F)

for (i in 1:length(pr.users.vec)) {
  user.id<-pr.users.vec[i]
  message(paste0("starting with user ", user.id))
  
  rl<- rate_limits(follower.sampler.token)
  if(any(rl$remaining<=2)){
    Sys.sleep(15*60)
  }
  
  followers<- get_followers(user = user.id,
                            n = 75000,
                            retryonratelimit = F,
                            parse = T,
                            token = follower.sampler.token)
  
  followers.vector<-as.vector(followers$user_id)
  
  if(nrow(followers)>=15000){
    
    message(paste0("user ", user.id," has more than 15.000 followers. Chunking the lookup!"))
    to.hydrate<- split(followers.vector, ceiling(seq_along(followers.vector)/15000))
    hydrated.followers<-tibble()
    for (j in 1:length(to.hydrate)) {
      to.hydrate.a<-unlist(to.hydrate[j])
      rl.two<- rate_limits(follower.sampler.token)
      #replace this condition with a specific reference to lookup user id
      if(isTRUE(any(rl.two$remaining<= 2))){
        Sys.sleep(15*60)
      }
        
      hydr<- lookup_users(to.hydrate.a,token = follower.sampler.token)
      hydrated.followers<-rbind(hydrated.followers,hydr)
    }
  open.hydrated<- dplyr::filter(hydrated.followers, protected == F)
  
  open.hydrated.sample<- slice_sample(open.hydrated, prop = .02)
  
  saveRDS(open.hydrated.sample,file = paste0(dataDIR,"follower_ids/","followers_",user.id,".RDS"))      
  
  }else{
    
    small.hydrated<- lookup_users(followers.vector,token = follower.sampler.token)
    
    open.small.hydrated<- dplyr::filter(small.hydrated, protected == F)
    
    open.small.hydrated.sample<- slice_sample(open.small.hydrated, prop = .02)
    
    saveRDS(open.small.hydrated.sample,file = paste0(dataDIR,"follower_ids/","followers_",user.id,".RDS"))      
    }

}

