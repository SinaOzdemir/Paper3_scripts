
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
      user_followers_data <- lookup_users(users = user_followers.uniq,parse = T,token = token)
      user_followers_data.open<- dplyr::filter(user_followers_data,protected == F)
      
      #sample 2% of the followers
      user_followers_sample<- slice_sample(.data = user_followers_data.open,
                                           prop = samp.prop)
      #put the system to sleep
      
      #return the data frame
      return(user_followers_sample)
    
    
  },warning = function(w){
    message(w)
  },error = function(err){
    e.message<-paste(Sys.time(),user_id, err, sep = "\n")
    write(x = e.message,file = paste0(dataDIR,"follower_sample_errors.txt"),append = T)
  }
  )
}
