dataDIR<-paste0(getwd(),"/Data/Clean_followers")
saveDIR <- paste0(getwd(),"/Data/")

replier_ids<-readRDS(paste0(saveDIR,"prof_to_scrape.RDS"))

reader.func<- function(x){
  data<-readRDS(x)
  unique_users<- dplyr::filter(data, !(user_id%in%replier_ids))
  
  if((nrow(unique_users)*.02) <= 1){
    user_ids<-unique_users$user_id
  }else{
    sample_data<- dplyr::slice_sample(.data = unique_users,prop = .02)
    user_ids<- sample_data$user_id
  }
  
  return(user_ids)
  
}


f.list<-list.files(path = dataDIR,pattern = "*.RDS",full.names = T)

follower_ids<- lapply(X = f.list, reader.func)

follower_ids_vec <- unlist(follower_ids)

saveRDS(object = follower_ids_vec,file = paste0(saveDIR,"/followers_to_scrape.RDS"))

