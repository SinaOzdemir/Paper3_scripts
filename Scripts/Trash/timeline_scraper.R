#rinse and repeat to scrape jth users followers
print(paste0("scraping the followers of ",j,"th replier"))

rl_follower = filter(rl,query%in% c("application/rate_limit_status","followers/ids"))


if (isTRUE(rl_follower[1,3] <= 2 | rl_follower[2,3] <= 2)) {
  
  print(x = paste0("Follower's ",j," is out of limit and waiting for 15 minutes at ", Sys.time()))
  
  Sys.sleep(time = (15*60))}

user<-lookup_users(users = replierIDunlist[j],parse = T,token = profileScraper)

if(user$follower_count <=5000){
  replierProfileFOLLOWERS[[j]] = tryCatch(expr = get_followers(user = replierIDunlist[j],
                                                               n = 5000,parse = T,
                                                               verbose = F,
                                                               token = profileScraper),
                                          warning = function(w){
                                            message(w)},
                                          error = function(e){
                                            print(paste0("something has gone pearshaped with scraping the followers of ", j,"th user!\ninitiating defensive protocol"))
                                            saveRDS(object = replierProfileFOLLOWERS,
                                                    file = paste0(followerSaveDIR,"user_followers",k,dates[i],"_",Sys.time(),".RDS"))
                                            Sys.sleep(10*60)
                                            j<-j-1
                                          },
                                          finally = {
                                            message(paste0(j,"th user's followers are scraped, moving on to their friends"))
                                          })}else{print(paste0(j,"th user has more than 5000 followers"))}
#again save follower lists every 1000 user
#each element of a list contains the follower ids of jth user
#for example replierProfileFOLLOWERS[1] will have the followers of replierIDunlist[1]
if (j == nextChunkCond) {
  
  print(paste0("saving the followers of ", j, "th profile of ", dates[i]))
  
  saveRDS(object = replierProfileFOLLOWERS, file = paste0(followerSaveDIR,"user_followers_",j,"_",dates[i],".RDS"))
  
  remove(replierProfileFOLLOWERS)
  
  replierProfileFOLLOWERS = vector("list",length = 1000)
  
}

#rinse and repeat to scrap jth users friends

print(paste0("scraping the friends of ",j,"th replier of ", dates[i]))

rl_friends = filter(rl, query %in%c("application/rate_limit_status","friends/ids"))


if (isTRUE(rl_friends[1,3] <= 2 | rl_friends[2,3] <= 2)) {
  
  print(x = paste0("Follower's ",j," is out of limit and waiting for 15 minutes at ", Sys.time()))
  
  Sys.sleep(time = (15*60))}

# same problem as followers

repliersProfileFRIENDS[[j]] = tryCatch(expr = rtweet::get_friends(user = replierID[j],n = 5000,parse = T,token = profileScraper),
                                       warning = function(w){
                                         message(w)},
                                       error = function(e){
                                         print(paste0("something has gone pearshaped with scraping friends of",j,"th user!\ninitiating defensive protocol"))
                                         saveRDS(object = repliersProfileFRIENDS,
                                                 file = paste0(friendsSaveDIR,"user_friends_",k,"_",Sys.time(),"_.RDS"))
                                         Sys.sleep(10*60)
                                         j<-j-1
                                       },
                                       finally = {
                                         message(paste0(j,"th user's friends are scraped"))
                                         message(paste0(j,"th user is complete, moving on to",j+1,"th user"))
                                       })

##save user friends in intervals of 1000 users, then remove the object for next 1000 so that it doesn't consume the memory

if (j == nextChunkCond) {
  
  print(paste0("saving the friends of ", j, "th profile of ", dates[i]))
  
  saveRDS(object = repliersProfileFRIENDS, file = paste0(friendsSaveDIR,"user_friends_",j,"_",".RDS"))
  
  remove(repliersProfileFRIENDS)
  
  repliersProfileFRIENDS = vector("list",length = 1000)
  
}
