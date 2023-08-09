test.id<- eu.profs %>% filter(user_id == "19722343")
previous.id<-readRDS(file = paste0(dataDIR,"prof_to_scrape.RDS"))

#I should take the whole list and then sample from the list


#no try catch
follower_sampler<- function(userdata,token,samp.prop,previous.ids){
  
  tv<-require(tidyverse)
  rt<-require(rtweet)
  
  if(isFALSE(tv)){
    
    install.packages("tidyverse")
    library(tidyverse)
    
  }else if(isFALSE(rt)){
    
    install.packages("rtweet")
    library(tweet)
    
  }else{
    
    packs<-c("tidyverse","rtweet")
    lapply(packs, library,character.only = T)
  }
  
  user_id<- userdata %>% select(user_id) %>% as.character()
  
  user_follower_count<- userdata %>% select(followers_count) %>% as.numeric()
  
  follower_sample_size<- round(user_follower_count*samp.prop)
  
  Sys.sleep(15*60)
  
  message(paste0("starting with the followers of ", user_id," it has ", user_follower_count," followers"))
 
    if(isTRUE(user_follower_count<=75000)){
      message("user has less than 75000 followers; starting to sample them")
      #look up the follower ids
      user_followers<-rtweet::get_followers(user = user_id,n = 75000,retryonratelimit = F,parse = T,verbose = F,token = token)
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
      message("sleeping before moving on to the next user")
      Sys.sleep(15*60)
      #return the data frame
      return(user_followers_sample)
      
      
    }#if the account has more than 75000 followers, scrape all the user ids then sample from them
    else{
      message("user has more than 75000 followers; starting to sample them. This may take a while")
      #start with the default page value
      cur_val<- "-1"
      #create a container for followers information
      rich_user_sample<-vector(mode = "character")
      #while the container length is smaller than the required sample size, repeat sampling the users from the same account
      while(length(rich_user_sample)<=user_follower_count){
        #look up user ids of the followers
        rich_user_followers_extended<-rtweet::get_followers(user = user_id,
                                                            n = 75000,
                                                            page = cur_val,parse = T,
                                                            token = token)
        #update the cursor value for page in order to proceed with the next batch
        
        if(isTRUE(nrow(rich_user_followers_extended)>0)){
        cur_val<-rtweet::next_cursor(rich_user_followers_extended)
        #check if I already have some of the followers 
        rich_user_followers_extended.vec <- as_vector(rich_user_followers_extended$user_id,"character")
        rich_user_followers_extended.unique<- rich_user_followers_extended.vec[-which(rich_user_followers_extended.vec%in%previous.ids)]
        #hydrate the users
          # rich_user_followers_extended.hyd<- lookup_users(users = rich_user_followers_extended.unique,
          #                                                 parse = T,
          #                                                 token= follower.sampler.token)
          # 
          # rich_user_followers_extended.open<- dplyr::filter(rich_user_followers_extended.hyd,
          #                                                   protected == F)
        #sample from the followers
        #populate the container
        rich_user_sample<-append(rich_user_sample,rich_user_followers_extended.unique)
        }
        
      }
      rich_user_sample_final<- slice_sample(.data = rich_user_sample,prop = samp.prop)
      #once done return the data on followers
      return(rich_user_sample_final)
      
    }
  }

#add a rate limit handler, without the rate limit handler, rtweet functions return a null object which then cannot be coerced to
#vector.

test_scrape<-follower_sampler(userdata = test.id,token = follower.sampler.token,samp.prop = .02,previous.ids = previous.id)

#lets try something else
#it works!

follower.sampler.token<-create_token(app = "follower_sampler",
                                     consumer_key = "8XdIXE4Ohb9E8ezo8U5ts1E8j",
                                     consumer_secret = "FK49c8YG7DqI5axCyUBdfCn0TrfKxQghqx9s84yjrlu7zHzOMo",
                                     access_token = "1151438784384421888-FVjuDOy97az8iBUuVOkZwp7Pyya3c4",
                                     access_secret = "VyJXEl8rxvKRFSW18uZdR2zpqcbW3NqTLOqZW3795wwwD",
                                     set_renv = F)


n_rep<- ifelse(is.integer(test.id$followers_count/75000),(test.id$followers_count/75000),ceiling(test.id$followers_count/75000))

curs_val<-"-1"

follower_container<-tibble()
rl<- rate_limits(token = follower.sampler.token)

for (i in 1:n_rep) {
  
  rl<- rate_limits(token = follower.sampler.token,)
  if(isTRUE(any(rl$remaining<=2))){
    message("rate limit is reached, waiting about 15 min")
    Sys.sleep(15*60)
  }
  test_scrape<-get_followers(user = test.id$user_id,n = 75000,retryonratelimit = F,page = curs_val,parse = T,token = follower.sampler.token)
  curs_val <- next_cursor(test_scrape)
  follower_container<-rbind(follower_container,test_scrape)
}