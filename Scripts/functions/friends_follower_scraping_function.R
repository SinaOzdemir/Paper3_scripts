# mid-level scraping functions --------------------------------------------

friends_scraper<- function(userids,token){
  user_friends<- get_friends(users = userids,n = 5000,retryonratelimit = F,token = token,parse = T)
  user_friends_vec<- user_friends %>% select(user_id) %>% as_vector(.x = .,.type = "character")
  user_friends_data<-lookup_users(users = user_friends_vec,parse = T,token = token)
  return(user_friends_data)
}

follower_scraper<-function(userids,token){
  user_followers<-get_friends(users = userids,n=5000,retryonratelimit = F,parse = T,token = token)
  user_followers_vector<- user_followers %>% dplyr::select(user_id) %>% as_vector(.x = .,.type = "character")
  user_followers_data<-lookup_users(users = user_followers_vector,parse = T,token = token)
  return(user_followers_data)
}

# high-level error proof --------------------------------------------------


user_ff_scraper<- function(id_chunk,saveDIR, token, chunk){
  
  
  user_ids<- unlist(id_chunk)
  
  user_followers<-vector(mode = "list",length = length(user_ids))
  user_friends<-vector(mode = "list",length = length(user_ids))
  print(paste0("starting followers from ",i,"th chunk at ", Sys.time()))
  j<-1
  while (j<=length(user_ids)) {
    print(paste0("starting the user ", j))
    rl = rate_limits(token = token)
    
    rl_tweets = filter(rl, query %in% c("application/rate_limit_status","followers/ids","friends/ids","users/lookup"))
    
    if (isTRUE(any(rl_tweets$remaining<=2))){
      
      Sys.sleep((15*60))
    }
    
    error_c<-F
    
    tryCatch(expr = {
      #get followers
      user_followers[[j]]<-follower_scraper(userids = user_ids[j],token = token)
      names(user_followers)[j]<- user_ids[j]
      
      #get friends
      user_friends[[j]]<-friends_scraper(userids = user_ids[j],token = token)
      names(user_friends)[j]<- user_ids[j]
    },warning = function(w){
      message(w)
    },
    error = function(e){
      error_c<-T
      saveRDS(user_followers,file = paste0(saveDIR,"Followers/","error_saves/","errored_follower_chunk.RDS"))
      saveRDS(user_friends, file = paste0(saveDIR,"Friends/","error_saves/","errored_friends_chunk.RDS"))
      error_log<- paste(Sys.time(),e,sep = "\n")
      write(x = error_log,file = paste0(saveDIR,"error_log.txt"),append = T)
      Sys.sleep(15*60)
    })
    
    if(isTRUE(j == length(user_ids))){
      saveRDS(user_followers,file = paste0(saveDIR,"Followers/","user_followers_",chunk,".RDS"))
      saveRDS(user_friends, file =  paste0(saveDIR,"Friends/","user_friends_",chunk,".RDS"))
    }
    
    if(isFALSE(error_c)){
      j<-j+1
    }else{
      j<-j
    }
    assign("user_followers_last_chunk", user_followers,envir = .GlobalEnv)
    assign("user_friends_last_chunk", user_friends,envir = .GlobalEnv)
  }
  
  
  print(paste0("chunk is completed at ", Sys.time()))
}
