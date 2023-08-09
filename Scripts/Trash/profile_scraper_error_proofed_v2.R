#to do:


packs = c("rtweet","tidyverse")

lapply(packs,library,character.only =T)

#create dirs

dataDIR = paste0(getwd(),"/Data/")

tweetSaveDIR = paste0(getwd(),"/Data/replier_tweets/")

followerSaveDIR =paste0(getwd(),"/Data/replier_followers/")

friendsSaveDIR = paste0(getwd(),"/Data/replier_friends/")

#create the twitter token

profileScraper = rtweet::create_token(app = "while_looped",
                                      consumer_key = "A0HWCtaUF5SNVOqwTLoKrAbWg" ,
                                      consumer_secret = "QRRVCTExg8mW6smHil6BVcP7Q1xKgpFHvdAVdhbRIWOxugOPSv",
                                      access_token = "1151438784384421888-K7Skj2sqiPHn1176tzKjKtwu54yGPu",
                                      access_secret = "8xp7B4DbBJmojGAjhguayi0AZBx1bTZ2gVwjMNLYvK3Iy",
                                      set_renv = F)

#create files

prof_to_download<- readRDS(file = paste0(dataDIR,"prof_download.RDS"))
#split the full list of users into chunks of 1000 so that I can save at the end of every chunk
replierIDlist = split(prof_to_download, ceiling(seq_along(prof_to_download)/1000))

#create lists to store the results
#I populated the results lists with logical condition for two reasons
#Firstly, I didn't want to increase the length of the list in the loop since it is very taxing
#Secondly, because I kept getting subscript out of bounds error, I decided to avoid populating
#the lists with NA or NULL just in case it is what causing the problem. Logicals seems to be the least RAM demanding
#data objects in R so it doesn't really cause much problem, and it plays nicely with if-else conditions

replierProfileTWEETS = as.list(rep(T,times = nextChunkCond))

replierProfileFOLLOWERS = as.list(rep(T,times = nextChunkCond))

repliersProfileFRIENDS = as.list(rep(T,times = nextChunkCond))


#scraping starts here
for (k in 1:length(replierIDlist)){
  print(paste0("starting the chunk", k," out of ", length(replierIDlist)))
  
  #unlist the first chunk of user ID
  replierIDunlist = unlist(replierIDlist[[k]])
  #create "the loop" variable
  j <- 1
  #set up the condition to finish the while
  nextChunkCond<- length(replierIDunlist)
  
  #I use while condition to be able to restart the scraping process
  #when there is an error. The logic is while j is smaller
  #than the length of user id vector, keep scraping the profiles,
  #after each successful scraping increase j by 1
  #tryCatch interrupts this process in case of an error
  #and sets j back to j-1 so that the while loop can try to scrape jth profile again
 
  
  while (j<=nextChunkCond) {
    
    #set up the error conditions
    #so long as they are true, it means they are fine
    user_timeline_state<- TRUE
    user_friends_state <- TRUE
    user_follower_state <- TRUE


# twitter users timelines -------------------------------------------------

    
    
        
    print(paste0("Scraping the tweets of ", j,"th replier"))
    
    #if condition to accommodate the api limits
    rl = rate_limits(token = profileScraper)
    
    rl_tweets = filter(rl, query %in% c("application/rate_limit_status","statuses/user_timeline","followers/ids","friends/ids"))
    #I am giving some elbow room to query limit so as not to get nasty warnings
    if (isTRUE(any(rl_tweets$remaining<=2))){
      print(paste0(j," is out of limit and waiting for 15 minutes at ",Sys.time()))
    
      Sys.sleep((15*60))
    }
    
    #if the last element is true, do the operation
    #if it is something else, say a data frame, if condition 
    #allows the flow to go on the next operation.
    #I did this so that, if scraping tweets of jth profile succesfull but 
    #scraping followers or friends is unsuccessful, 
    if (isTRUE(replierProfileTWEETS[[j]])) {
      
      #this is the main bit
      replierProfileTWEETS[[j]] <- tryCatch(
        #get the user data
        expr = get_timeline(user = replierIDunlist[j],
                            n = 3200,parse = T,
                            token = profileScraper),
                                            #in case of a warning such as user profile is private etc,
                                            #it just prints the warning on the console
                                           warning = function(w){
                                             message(w)
                                             return(TRUE)},
                                          #In case of an error  
                                           error = function(e){
                                             #print a message on the console something gone wrong
                                             print(paste0("something has gone pearshaped with scraping tweets of",j,"th user! Initiating defensive protocol"))
                                             #change the error condition to false so that while loop can reset
                                             user_timeline_state<- FALSE
                                             #save the error log
                                             error_log<-paste(Sys.time(),e,sep = "\n")
                                             
                                             write(x = error_log,file = paste0(tweetSaveDIR,"/error_save/","error_log.txt"),append = T)
                                             #save the downloaded data
                                             #Probable cause of the error
                                             timelineSolidState<- replierProfileTWEETS
                                             
                                             saveRDS(object = timelineSolidState,
                                                     file = paste0(tweetSaveDIR,"/error_save/","user_tweets_chunk_",k,"_user_",j,".RDS"))
                                             #revert the last element of the result list to true
                                             #for the next try
                                             return(TRUE)
                                             #put the system to sleep.
                                             #So far I encountered regular errors from get_timeline()
                                             #First one is API related. Sometimes twitter api returns a bad request
                                             #Second one is simply internet connect, sometimes it goes offline
                                             #Last type is R's own problem with curl, simply saying fetch_memory_fail
                                             #Related answers on stackexhange pointed towards the data size,
                                             #since I am working in chunks of users, this should no longer be a problem
                                             #So tryCatch is here to handle the first two type of errors
                                             Sys.sleep(15*60)
                                             
                                           })
    }
    
    

    
   
    #if the error condition is F, meaning that there was an error,
    #this simply reverts the loop variable j back to j so to invoke restart
    if (isFALSE(user_timeline_state)) {
      print(paste0("there was an error in scraping user ",replierIDunlist[j]," timeline"))
      
      print(paste0("will retry scraping the timeline of ",replierIDunlist[j]))
      
      j<-j
      
      
    }
    
    #here if the first chunk of 1000 users are scraped, I save the data and recreate the list object
    #so it doesn't consume too much memory
    if (j == nextChunkCond) {
      
      print(paste0("saving the tweets from",k,"th batch"))
      
      saveRDS(object = replierProfileTWEETS,file = paste0(tweetSaveDIR,"user_tweets_",k,".RDS"))
      
      remove(replierProfileTWEETS)
      
      replierProfileTWEETS = as.list(rep(T,times = nextChunkCond))
    }
    

    
#Rinse and repeat the previous steps to get user's friends    
# Twitter user friends: -------------------------------------------------

    print(paste0("scraping the friends of ",j,"th replier of "))
    
   
    #scrape the user's friends
    #if 
    if(isTRUE(repliersProfileFRIENDS[[j]])){
    
      repliersProfileFRIENDS[[j]] = tryCatch(expr = rtweet::get_friends(user = replierIDunlist[j],n = 5000,parse = T,token = profileScraper),
                                             warning = function(w){
                                               message(w)},
                                             error = function(e){
                                               print(paste0("something has gone pearshaped with scraping friends of",j,"th user! initiating defensive protocol"))
                                               
                                               error_log<-paste(Sys.time(),e,sep = "\n")
                                               
                                               write(x = error_log,file = paste0(tweetSaveDIR,"/error_save/","error_log.txt"),append = T)
                                              
                                               user_friends_state <- F
                                               
                                               friendsSolidState<- repliersProfileFRIENDS
                                               
                                               saveRDS(object = friendsSolidState,
                                                       file = paste0(friendsSaveDIR,"/error_save/","user_friends_chunk",k,"_user_",j,".RDS"))
                                               
                                               
                                               return(T)
                                               
                                               Sys.sleep(15*60)
                                               }
                                             )
      
        
    }
    
    
    #error check on friends scraping
   
    
    if (isFALSE(user_friends_state)){
      print(paste0("there was an error in scraping user ",replierIDunlist[j]," friend"))
      print(paste0("will retry scraping the friends of ",replierIDunlist[j]))
      
      j<-j
    }
    
    
    ##save user friends in intervals of 1000 users, then remove the object for next 1000 so that it doesn't consume the memory
    
    if (j == nextChunkCond) {
      
      print(paste0("saving the friends of ", j, "th profile of ", dates[i]))
      
      saveRDS(object = repliersProfileFRIENDS, file = paste0(friendsSaveDIR,"user_friends_",k,".RDS"))
      
      remove(repliersProfileFRIENDS)
      
      repliersProfileFRIENDS = as.list(rep(T,times = nextChunkCond))
      
    }

#Repeating the same steps for users followers
# scraping users followers ------------------------------------------------

    print(paste0("scraping the followers of ",j,"th replier"))
    
   
    if(isTRUE(replierProfileFOLLOWERS[[j]])){
      replierProfileFOLLOWERS[[j]] <- tryCatch(expr = get_followers(user = replierIDunlist[j],
                                                                   n = 5000,parse = T,
                                                                   verbose = F,
                                                                   token = profileScraper),
                                              warning = function(w){
                                                message(w)},
                                              error = function(e){
                                                print(paste0("something has gone pearshaped with scraping the followers of ", j,"th user! Initiating defensive protocol"))
                                                
                                                error_log<-paste(Sys.time(),e,sep = "\n")
                                                
                                                write(x = error_log,file = paste0(getwd(),"/error_log.txt"),append = T)
                                                
                                                user_follower_state <- F
                                                
                                                followerSolidState<- replierProfileFOLLOWERS
                                                
                                                saveRDS(object = followerSolidState,
                                                        file = paste0(followerSaveDIR,"/error_save/","user_followers_chunk_",k,"_user_",j,".RDS"))
                                                
                                               
                                                return(T)
                                                
                                                Sys.sleep(15*60)
                                                
                                              })}
    
    #error check
    
    
    if(isFALSE(user_follower_state)){
      
      print(paste0("there was an error in scraping user ",replierIDunlist[j]," followers"))
      
      print(paste0("will retry scraping the followers of ",replierIDunlist[j]))
      
      j<-j
    }
    
    if (j == nextChunkCond) {
      
      print(paste0("saving the followers for",k,"th batch"))
      
      saveRDS(object = replierProfileFOLLOWERS,file = paste0(followerSaveDIR,"user_followers_",k,".RDS"))
      
      remove(replierProfileFOLLOWERS)
      
      replierProfileFOLLOWERS = vector("list",length = 1000)
    }
    
    
    #if all operations are successful move on to the next iteration
    if(isTRUE(all(user_timeline_state,user_friends_state,user_follower_state))){
      print(paste0("all scraping operations are succesfull. Moving on to user ", replierIDunlist[j]))
      j<-j+1
    }
    
    
    }
    }


