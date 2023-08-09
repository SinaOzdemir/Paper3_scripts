
user_tweets_scraper<- function(id_chunk,saveDIR, token, chunk){
  
  
  user_ids<- unlist(id_chunk)
  
  user_tweets<-vector(mode = "list",length = length(user_ids))
  print(paste0("starting tweets from",i," th chunk"))
  j<-1
  while (j<=length(user_ids)) {
    
    rl = rate_limits(token = profileScraper)
    
    rl_tweets = filter(rl, query %in% c("application/rate_limit_status","statuses/user_timeline"))
    
    if (isTRUE(any(rl_tweets$remaining<=2))){
      
      Sys.sleep((15*60))
    }
    
    error_c<-F
    
    tryCatch(expr = {
      
      user_tweets[[j]]<- rtweet::get_timeline(user = user_ids[j],n = 3200,parse = T,token = token)
      
    },
    error = function(e){
      error_c<<-T
      saveRDS(user_tweets,file = paste0(saveDIR,"error_saves/","errored_chunk.RDS"))
      error_log<- paste(Sys.time(),e,sep = "\n")
      write(x = error_log,file = paste0(saveDIR,"error_logs/error_log.txt"),append = T)
      Sys.sleep(15*60)
    })
    
    if(j == length(user_ids)){
      saveRDS(user_tweets,file = paste0(saveDIR,"user_data_",chunk,".RDS"))
    }
    
    if(isFALSE(error_c)){
      j<-j+1
    }else{
      j<-j
    }
    
  }
  
  assign("user_tweets_last_chunk",user_tweets,envir = .GlobalEnv)
  print(paste0("chunk is completed"))
}
