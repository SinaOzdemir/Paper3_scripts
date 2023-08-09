#functionized scraper


packs = c("rtweet","tidyverse")

lapply(packs,library,character.only =T)

#create dirs

dataDIR = paste0(getwd(),"/Data/")

tweetSaveDIR =  paste0(dataDIR,"follower_tweets/")

funcDIR = paste0(getwd(),"/Scripts/functions/")

source(file = paste0(funcDIR,"tweet_scraping_function.R"))

#create the twitter token

profileScraper = rtweet::create_token(app = "Functionised_collector",
                                      consumer_key = "3VPlucybBPiPFG6KcaEIp4EaJ" ,
                                      consumer_secret = "IFXE6Whil5uLxFuGzLHbnIQcmavZGNLYU00uQdcBGo1HeZtcpC",
                                      access_token = "1151438784384421888-k1agQ0ObE70SjbL5FYR3Jz8FtrGuba",
                                      access_secret = "qEygp9CQ7U0MGwBGPBQ2jzZArTuxbV7h2x2N5BHzG9HB2",
                                      set_renv = F)

#create files

prof_to_download<- readRDS(file = paste0(dataDIR,"followers_to_scrape.RDS"))
#split the full list of users into chunks of 1000 so that I can save at the end of every chunk

replierIDlist = split(prof_to_download, ceiling(seq_along(prof_to_download)/100))




#loop, also now I can use for each with this

for (i in 1410:length(replierIDlist)) {
  user_tweets_scraper(id_chunk = replierIDlist[i],saveDIR = tweetSaveDIR,token = profileScraper,chunk = i)
}
