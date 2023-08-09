dataDIR<-paste0(getwd(),"/Data/follower_tweets")

files<-list.files(path = dataDIR,pattern = "*.RDS",full.names = T)

a<-readRDS(files[1])

list.meta<-length(a)
b<-a[[1]][1,]

user_meta_data<-tibble()

for (i in 1:length(a)) {
  data.size[i]<- nrow(a[[i]])
}


empty_prof_count<-length(data.size[which(data.size==0)])

data_checker <- function(data.directory){
  data<-readRDS(data.directory)
  for (i in 1:length(data)) {
    data.size[i]<-nrow(data[[i]])
    
  }
  data.meta<-data.frame(file.name = gsub("C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/follower_tweets/","",data.directory),
            emtpy_user = length(data.size[which(data.size == 0)]),
            tweet_count = sum(data.size))
  
  return(data.meta)
  
}


random.check.sample<- sample(files,size = (length(files)*.1),replace = F)

meta.sample <- data.frame()

for (j in 1:length(random.check.sample)) {
  a<- data_checker(random.check.sample[j])
  meta.sample<-rbind(meta.sample,a)
}

ggplot(data = meta.sample,aes(x = file.name, y = emtpy_user))+geom_bar(aes(fill = emtpy_user),stat = "identity", position = "dodge")

ggplot(data = meta.sample,aes(x = file.name, y = tweet_count))+geom_bar(aes(fill = emtpy_user),stat = "identity", position = "dodge")
