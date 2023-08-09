#replier and retweeter sampling
library(tidyverse)
eu.tw.sample<-readRDS(file = file.choose())

eu.twt.id<- eu.tw.sample %>% select(status_id) 
  as_vector(.type = "character")
  
eu.twt.id<-str_remove(string = eu.twt.id$status_id,pattern = "x")

replier_files<-list.files(path = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/phd data/Phd_data_collection/Scripts/extended_list_collection/replies_to_actors",
                          pattern = "*.RDS",full.names = T)

replier_files<-replier_files[-which(grepl(x = replier_files,pattern = "ascii",fixed = T))]

#just test with the coding sample

replies<-tibble()

for (i in seq_along(replier_files)) {
  a<-readRDS(replier_files[i])
  b<-filter(a,reply_to_status_id%in%eu.twt.id)
  replies<-rbind(replies,b)
  remove(list = c("a","b"))
  
}

replier.id<-select(replies,user_id) %>% unique()

# retweeters

retweeter_files<-list.files(path = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/phd data/Phd_data_collection/Scripts/extended_list_collection/Retweeter_ids",pattern = "*.RDS",full.names = T)
retweeter_files<-retweeter_files[-which(grepl(pattern = "ascii",x = retweeter_files,fixed = T))]

retweeter.ids<-list()

for (i in seq_along(retweeter_files)) {
  a<-readRDS(retweeter_files[i])
  b<-a[which(names(a)%in%eu.twt.id)]
  retweeter.ids<-append(retweeter.ids,b)
  remove(list = c("a","b"))
}

retweeter.ids.unlist<-retweeter.ids %>% unlist()

retweeter.ids.unlist.unique<-retweeter.ids.unlist %>% unique()

prof.to.scrape<-append(retweeter.ids.unlist.unique,replier.id$user_id)

#check if some them are already scraped

scraped.profs<-readRDS(file = file.choose())

scraped.profs.a<-scraped.profs[1:53200]

prof.to.scrape.a<-prof.to.scrape[-which(prof.to.scrape %in% scraped.profs.a)]

saveRDS(object = prof.to.scrape.a,file = "prof_to_scrape.RDS")
