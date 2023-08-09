#Profile downloader
#things to do:

#1) last chunk of the replier ID list is not 1000, add an extra if condition to fix that
#2) add defensive programming practices to restrat the loop if there is an error
#Possible errors: disconnected from internet, curl related errors, API related errors
#if the error related to curl, or API try to restart the loop. If it is disconnection sleep the loop


#load packages

packs = c("rtweet","tidyverse")

lapply(packs,library,character.only =T)

#create dirs

dataDIR = paste0(getwd(),"/Data/")

tweetSaveDIR = paste0(getwd(),"/Data/replier_tweets/")

followerSaveDIR =paste0(getwd(),"/Data/replier_followers/")

friendsSaveDIR = paste0(getwd(),"/Data/replier_friends/")
 
#create the twitter token

profileScraper = rtweet::create_token(app = "profile_seagull",
                                      consumer_key = "U9s3YyMbmuPiGPWz5WXDFkG63" ,
                                      consumer_secret = "B6KW2NjY6xfId77BLSRJoy6DtJdi8qVpZYHS4m3KBVddP29SsX",
                                      access_token = "1151438784384421888-58LvzXLYYAiLpUbnZzvqSDiJmDNPJI",
                                      access_secret = "wRdDEE5WbmYO61Wp84XjkQcuqYBCpiCF2HHGUiHovLOkT",
                                      set_renv = F)

#create files

dates = readRDS(file = paste0(dataDIR,"dates.RDS"))

prof_to_download<- readRDS(file = paste0(dataDIR,"prof_download.RDS"))

replierProfileTWEETS = vector("list",length = 1000)

replierProfileFOLLOWERS = vector("list",length = 1000)

repliersProfileFRIENDS = vector("list", length = 1000)

replierIDlist = split(prof_to_download, ceiling(seq_along(prof_to_download)/1000))

#loopy loop

#number of unique repliers is not multiple of 1000, the last chunk wont trigger the save,
#add a second if condition that j == length(replierIDunlist[element]) save
start_time = Sys.time()

  for (k in 1:length(replierIDlist)) {
    
    print(paste0("starting the chunk ", k, " out of ", length(replierIDlist)))
    
    replierIDunlist = unlist(replierIDlist[[k]])
    
    #loop over the user IDs in a chunk
    for (j in 1:length(replierIDunlist)) {
      
      rl = rate_limits(token = profileScraper)
      
      print(paste0("Scraping the tweets of ", j,"th replier of ", dates[i]))
      
      rl_tweets = filter(rl, query %in% c("application/rate_limit_status","statuses/user_timeline"))

      if (isTRUE(rl_tweets[1,3] <= 2 | rl_tweets[2,3] <= 1)) {
        print(x = paste0(j," is out of limit and waiting for 15 minutes at ", Sys.time()))
        
        Sys.sleep(time = (15*60))}
      
      replierProfileTWEETS[[j]] = rtweet::get_timeline(user = replierIDunlist[j],
                                                       n = 3200,parse = T,token = profileScraper)
      
      
      
      if (isFALSE(is.null(replierProfileTWEETS[[length(replierProfileTWEETS)]]))) {
      
        print(paste0("saving the tweets of ", j,"th profile of ",dates[i]))
        
        saveRDS(object = replierProfileTWEETS,file = paste0(tweetSaveDIR,"user_tweets_",k,"_",dates[i],".RDS"))
        
        remove(replierProfileTWEETS)
        
        replierProfileTWEETS = vector("list",length = 1000)
    }
    
      
       #scrape replying users followers
      
       ##control condition to handle the API limits, puts the system to sleep if relevant API limits are reached
     
       print(paste0("scraping the followers of ",j,"th replier of ", dates[i]))
      
       rl_follower = filter(rl,query%in% c("application/rate_limit_status","followers/ids"))
      
      
       if (isTRUE(rl_follower[1,3] <= 2 | rl_follower[2,3] <= 1)) {
      
         print(x = paste0("Follower's ",j," is out of limit and waiting for 15 minutes at ", Sys.time()))
      
         Sys.sleep(time = (15*60))}
      
       # there is a tiny bit of problem here, limit of one querry is 5000 but it is possible to get 75,000 followers before waiting
       # for the API limits to refresh. I don't expect an average user to have more than 5000 replies but should that be the case
       # this script cannot get more than 5000 followers per user. Documentation says it can be done using page option which controls
       # the cursor values. I don't really know what any of that means so I can't implement a solution yet...

       replierProfileFOLLOWERS[[j]] = rtweet::get_followers(user = replierID[j],n = 5000,parse = T,token = profileScraper)
      
       ##save user followers in intervals of 1000 users, then remove the object for next 1000 so that it doesn't consume the memory
       ##then rinse and repeat
      
      
       if (isFALSE(is.null(replierProfileFOLLOWERS[[1000]]))) {
      
       print(paste0("saving the followers of ", j, "th profile of ", dates[i]))
      
         saveRDS(object = replierProfileFOLLOWERS, file = paste0(followerSaveDIR,"user_followers_",j,"_",dates[i],".RDS"))
      
         remove(replierProfileFOLLOWERS)
      
         replierProfileFOLLOWERS = vector("list",length = 1000)
      
       }
      
       #Scrape the user friends (those followed by the user)
      
       print(paste0("scraping the friends of ",j,"th replier of ", dates[i]))
      
       rl_friends = filter(rl, query %in%c("application/rate_limit_status","friends/ids"))
      
      
       if (isTRUE(rl_friends[1,3] <= 2 | rl_friends[2,3] <= 1)) {
      
         print(x = paste0("Follower's ",j," is out of limit and waiting for 15 minutes at ", Sys.time()))
      
         Sys.sleep(time = (15*60))}
      
       # same problem as followers
      
       repliersProfileFRIENDS[[j]] = rtweet::get_friends(user = replierID[j],n = 5000,parse = T,token = profileScraper)
      
       ##save user followers in intervals of 1000 users, then remove the object for next 1000 so that it doesn't consume the memory
       ##then rinse and repeat
      
      
       if (isFALSE(is.null(repliersProfileFRIENDS[[1000]]))) {
      
         print(paste0("saving the friends of ", j, "th profile of ", dates[i]))
      
         saveRDS(object = repliersProfileFRIENDS, file = paste0(friendsSaveDIR,"user_friends_",j,"_",dates[i],".RDS"))
      
         remove(repliersProfileFRIENDS)
      
         repliersProfileFRIENDS = vector("list",length = 1000)
      
      }

    }   
}
end_time = Sys.time()

print(paste0("start time: ", start_time," end time: ", end_time))
