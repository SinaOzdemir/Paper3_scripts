# clustering and description


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","patchwork","Rclusterpp"))#Rclusterpp requires devtools to install from github



# directories -------------------------------------------------------------

data_path<-"M:/cluster count/audience_clustering_data"

results_path<- "M:/cluster count/results"


# data --------------------------------------------------------------------

data<- readRDS(file = paste0(data_path,"/clustering_df_scaled.RDS"))


# clusterings -------------------------------------------------------------
##k means test
km_clusters<-data.frame()

for (i in 1:10) {
  km<- kmeans(data,centers = i,nstart = 100)
  cluster_stat<-data.frame(cluster_n = i,
                           betweenss = km$betweenss,
                           totss = km$totss)
  km_clusters<-rbind(km_clusters,cluster_stat)
}

km_clusters<- km_clusters %>% 
  mutate(var_explained = betweenss/totss)

km_clusters %>% 
  ggplot(aes(x = as.factor(cluster_n),y = (1-var_explained),group = 1))+
  geom_point(aes(size = var_explained),color = "steelblue",show.legend = F)+
  geom_line(color = "steelblue")+
  theme_bw()+
  labs(x="cluster n", y = "% variation unexplained")
## kmeans

kmeans_n3<- kmeans(x = as.matrix(data),centers = 3)

centers<- kmeans_n3$centers

clusters<- data.frame(user_id = names(kmeans_n3$cluster),
                      cluster = kmeans_n3$cluster)

save(kmeans_n3,centers,clusters,file = paste0(results_path,"/kmeans_n3_clustering_results.Rdata"))



# hierarchical clustering -------------------------------------------------
hierarchical_clustering_data<- vector(mode = "list",length = 10)

for (i in 1:length(hierarchical_clustering_data)) {
  
  if(i==10){
   hierarchical_clustering_data[[i]]<-data 
  }
  
  sample_id<- sample(1:nrow(data),size = 21050,replace = F)
  sample_group<- data[sample_id,]
  hierarchical_clustering_data[[i]]<-sample_group
  data<-data[-sample_id,]
  
}





## hierarchical - average

haverage_n2<- list()
for (i in 1:length(hierarchical_clustering_data)) {
  cat("average - clustering user batch: ",i,"\n")
  haverage_n2[[i]]<-Rclusterpp.hclust(x = as.matrix(hierarchical_clustering_data[[i]]),method = "average")
  saveRDS(haverage_n2,file = paste0(results_path,"/haverage_n2_results.RDS"))
}

## hierarchical - complete


hcomplete_n2<- list()
for (i in 1:length(hierarchical_clustering_data)) {
  cat("complete - clustering user batch: ",i,"\n")
  hcomplete_n2[[i]]<-Rclusterpp.hclust(x = as.matrix(hierarchical_clustering_data[[i]]),method = "complete")
  saveRDS(hcomplete_n2,file = paste0(results_path,"/hcomplete_n2_results.RDS"))
}

## hierarchical - single

hsingle_n2<- list()
for (i in 1:length(hierarchical_clustering_data)) {
  cat("complete - clustering user batch: ",i,"\n")
  hsingle_n2[[i]]<-Rclusterpp.hclust(x = as.matrix(hierarchical_clustering_data[[i]]),method = "single")
  saveRDS(hsingle_n2,file = paste0(results_path,"/hsingle_n2_results.RDS"))
}

## hierarchical - ward

hward_n2<- list()
for (i in 1:length(hierarchical_clustering_data)) {
  cat("complete - clustering user batch: ",i,"\n")
  hward_n2[[i]]<-Rclusterpp.hclust(x = as.matrix(hierarchical_clustering_data[[i]]),method = "ward")
  saveRDS(hward_n2,file = paste0(results_path,"/hward_n2_results.RDS"))
}

# Dendograms
## its not really possible to make sense of dendograms because there are too many observations and the aggregation
## is pretty dense. Cutting it is not really helpful eather
p_load(char = "dendextend")

example_cluster<- haverage_n2[[1]]

dend<- rotate(dend,1:21050)

dend<-color_branches(dend, k = 2)


plot(dend)

