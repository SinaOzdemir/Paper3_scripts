#functionized scraper


packs = c("rtweet","tidyverse")

lapply(packs,library,character.only =T)

#create dirs

dataDIR = paste0(getwd(),"/Data/")
#savefolder fllowers_friends referst to the followers and friends of repliers!!!!!
saveDIR <- paste0(dataDIR,"EU_followers/")

funcDIR = paste0(getwd(),"/Scripts/functions/")

source(file = paste0(funcDIR,"friends_follower_scraping_function.R"))

#create the twitter token

friendFollowerScraper = rtweet::create_token(app = "lenovo-ideapad-ff-scraper",
                                      consumer_key = "0xKqiQjweuBC2OCfOk3Ja1qbu" ,
                                      consumer_secret = "zXKqDr4k5Tv6YDUC7pn7UdkAehXywJMzjAuOiiGLnQZpEzzXiB",
                                      access_token = "1151438784384421888-RIw38I5DVsSBEl2qNHRX4WUoTG36Ip",
                                      access_secret = "yMnWlFQzTxVKU7ji0lghoZbxA3lFl3H7IP0FTBF4Mdhps",
                                      set_renv = F)

#create files

prof_to_download<- readRDS(file = paste0(dataDIR,"followers_to_scrape.RDS"))
#split the full list of users into chunks of 1000 so that I can save at the end of every chunk
replierIDlist = split(prof_to_download, ceiling(seq_along(prof_to_download)/100))
remove(prof_to_download)


for (i in 396:478) {
  user_ff_scraper(id_chunk = replierIDlist[i],saveDIR = saveDIR,token = friendFollowerScraper,chunk = i)
}