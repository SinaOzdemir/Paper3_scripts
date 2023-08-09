#optimum number of clusters summary



# setup ------------------------------------------------------------------

library(pacman)
p_load(char = c("tidyverse","patchwork"))

# paths -------------------------------------------------------------------

data_path<- "M:/cluster count/audience_clustering_data"

results<- list.files(path = data_path,pattern = "optimum",full.names = T)

algo_name<- gsub("M:/cluster count/audience_clustering_data/optimum_cluster_number_|.RDS","",results)

algo_name[3]<- "ward.D2"

# summary -----------------------------------------------------------------

summary_data<- data.frame()
for (i in 1:length(results)) {
  result<- readRDS(file = results[i])
  aggregate_result<- result %>%
    group_by(index,best_nc) %>% 
    tally() %>% 
    ungroup() %>% 
    mutate(perc = n/100) %>% 
    group_by(index) %>% 
    filter(perc == max(perc)) %>%
    mutate(algorithm = algo_name[i]) %>% 
    ungroup()
  
  summary_data<- rbind(summary_data,aggregate_result)

}

summary_data<- summary_data %>% 
  mutate(algorithm = ifelse(algorithm == "kmeans",algorithm,paste0("hierarchical - ",algorithm)))

summary_bestnc<- summary_data %>%
  group_by(best_nc,algorithm) %>%
  tally() %>% 
  mutate(best_nc = as.factor(best_nc))


by_algo<-summary_bestnc %>%
  filter(algorithm == "kmeans") %>% 
  ggplot(aes(x = best_nc,y = n))+
  geom_bar(fill = "steelblue", position ="dodge",stat = "identity")+
  theme_bw()+
  labs(x= "Cluster number",y = "Vote count",title = "Optimum number of clusters for Kmeans")+
  scale_y_continuous(breaks = seq(0,10,1))
  facet_grid(~algorithm)

overall<- summary_bestnc %>% 
  group_by(best_nc) %>% 
  tally() %>% 
  ggplot(aes(x = best_nc,y = n))+
  geom_bar(fill = "steelblue",position = "dodge", stat = "identity")+
  theme_bw()+
  labs(x = "Cluster number", y= "Vote count", title = "Overall optimum number of clusters")

overall/by_algo
