## user csv.


# setup -------------------------------------------------------------------

#packages

library(pacman)

p_load(char = c("tidyverse","feather","emo","qdapRegex"))


# dirs --------------------------------------------------------------------

engager_dir<- "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/replier_tweets"
  
follower_dir<- "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/follower_tweets"

user_files<- c(list.files(path = engager_dir,pattern = "*.RDS",full.names = T),
               list.files(path = follower_dir,pattern = "*.RDS",full.names = T))

save_dir<- "M:/paper3_audience_clustering_data/user_csv/users.txt"

dir_condition = file.exists(save_dir)

# if (isFALSE(dir_condition)) {
#   file.create(save_dir)
# }
# task --------------------------------------------------------------------

for (i in 1266:length(user_files)) {
  cat("processing user batch ",i,"\n")
  
  user_batch<- readRDS(user_files[i])
  
  for (j in 1:length(user_batch)) {
    user_data<- user_batch[[j]]
    
    if(nrow(user_data)>0){
      
      user_aggregate <-user_data %>%  select(user_id,text) %>% 
        mutate(text =  qdapRegex::rm_url(text),
               text = qdapRegex::rm_twitter_url(text),
               text = str_replace_all(text,pattern  ="&amp",replacement = "and"),
               text = str_remove_all(pattern = emo::ji_rx,text),
               text = str_remove_all(pattern = "\n|\t|\"|\'|[:punct:]",text)) %>% 
        group_by(user_id) %>% 
        summarise(tweets = paste(text,collapse = ". "))
      
      
      write.table(x=user_aggregate,
                  file = save_dir,
                  append = T,
                  quote = T,
                  sep = ",",
                  na = "NA",
                  col.names = F,
                  row.names = F,
                  fileEncoding = "UTF-8")  
      
    
      
      
      
    }else{
      cat("user data ",i,"-",j," is empty\n")
      next
    }
  }
  
}

  