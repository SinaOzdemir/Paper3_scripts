#cluster examination:


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse"))


# paths -------------------------------------------------------------------

user_file = "M:/cluster count/audience_clustering_data"

results = "M:/cluster count/results"


# data sets ---------------------------------------------------------------


user_data_raw = readRDS(file = paste0(user_file,"/clustering_data_raw.RDS"))

user_data_scaled = readRDS(file = paste0(user_file,"/clustering_df_scaled.RDS"))

load(file = paste0(results,"/kmeans_n3_clustering_results.Rdata"))
kmeans()

c<- kmeans_n3$totss
# cluster visualization ---------------------------------------------------

pca_users = princomp(x = user_data_scaled)

two_component_solution = as.data.frame(pca_users$scores[,1:2]) %>% 
  rownames_to_column(var = "user_id")

graph_data = two_component_solution %>% 
  left_join(.,clusters,by = "user_id")

kmeans_clust = graph_data %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  ggplot(aes(x = Comp.1, y = Comp.2))+
  geom_point(aes(color = cluster, shape = cluster))+
  theme_bw()+
  labs(title = "3 Cluster solution of EU executives' audience (Kmeans)",subtitle = paste0("N of users: ",nrow(graph_data)))


# centroids ---------------------------------------------------------------
distance_matrix<- matrix(nrow = nrow(centers),ncol = nrow(user_data_scaled))

for (i in 1:nrow(centers)) {
  for(j in 1:nrow(user_data_scaled)){
    dist_mat = as.matrix(rbind(as.vector(centers[i,],mode = "numeric"),as.vector(user_data_scaled[j,],mode = "numeric")))
    distance_matrix[i,j] = as.matrix(dist(dist_mat))[1,2]
  }
  
}

dist_df<- as.data.frame(distance_matrix)
saveRDS(dist_df,file = paste0(results,"/kmeans_distance_values.RDS"))

cluster_density<- readRDS(paste0(results,"/kmeans_distance_values.RDS")) %>%
  rownames_to_column(var = "cluster") %>% 
  pivot_longer(cols = V1:V210504,names_to = "user",values_to = "distance") %>% 
  group_by(cluster) %>% 
  summarise(avg_distance = round(mean(distance),2)) %>% 
  ggplot(aes(x = cluster,y=avg_distance))+
  geom_bar(aes(fill = avg_distance),stat ="identity", position = "dodge",show.legend = F)+
  theme_bw()+
  labs(x = "Cluster ID", y = "Average distance(Euclidean)",subtitle = "average distance of observations from the cluster center")

rownames(dist_df)<-c("cluster_1","cluster_2","cluster_3")

cluster_cores <- dist_df %>% 
  rownames_to_column(var = "cluster") %>% 
  pivot_longer(cols = V1:V210504,names_to = "users",values_to = "eucl_distance") %>% 
  group_by(cluster) %>% 
  slice_min(order_by = eucl_distance,n = 10)


cluster_extremities <- dist_df %>% 
  rownames_to_column(var = "cluster") %>% 
  pivot_longer(cols = V1:V210504,names_to = "users",values_to = "eucl_distance") %>% 
  group_by(cluster) %>% 
  slice_max(order_by = eucl_distance,n = 10)

saveRDS(cluster_cores,file = paste0(results,"kmeans_cluster_cores.RDS"))
saveRDS(cluster_extremities,paste0(results,"kmeans_cluster_extremities.RDS"))

core_user_coords<- as.numeric(gsub("V","",cluster_cores$users))

core_user_ids<-rownames(user_data_scaled)[core_user_coords]

extremity_user_ids<- rownames(user_data_scaled)[as.numeric(gsub("V","",cluster_extremities$users))]

core_users_df<- user_data_raw %>% 
  filter(user_id%in%core_user_ids) %>% 
  select(user_id:retweet_count)

clusters<-data.frame(user_id = names(kmeans_n3$cluster),
                     cluster = kmeans_n3$cluster)

core_users_df<- core_users_df %>% 
  left_join(.,clusters,by = "user_id")
saveRDS(core_users_df,file = paste0(results,"/kmeans_core_users_desc_df.RDS"))


# cluster core summaries --------------------------------------------------

get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

cluster_cores<- readRDS(file = paste0(results,"/kmeans_core_users_desc_df.RDS"))

polint_breadth<- cluster_cores %>% 
  group_by(cluster) %>% 
  summarise(mode_polint_breadt = get_mode(total_pol_res))

polint_depth<- cluster_cores %>% 
  pivot_longer(cols = Agriculture_Fisheries:Justice_Home_affairs,names_to = "policy_area",values_to = "interested") %>% 
  group_by(cluster,policy_area) %>% 
  summarise(interest_depth = sum(interested),
            interest_depth_perc = interest_depth/10)

#maybe median
engagement_behavior <- cluster_cores %>% 
  pivot_longer(cols = quote_count:retweet_count,names_to = "engagement", values_to = "count") %>% 
  group_by(cluster,engagement) %>% 
  summarise(average_engagement = mean(count))

library(patchwork)

polint_breadt_graph<- polint_breadth %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  ggplot(aes(x = cluster, y = mode_polint_breadt))+
  geom_bar(aes(fill = cluster),stat = "identity", position = "dodge")+
  theme_bw()+
  labs(x = "Cluster ID", y = "Mode Interest breadth",title = "Interest breadth of users by cluster")

polint_debth_graph<- polint_depth %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  ggplot(aes(x = policy_area, y = interest_depth_perc))+
  geom_bar(aes(fill = interest_depth_perc),stat = "identity",position = "dodge")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "policy area", y = "percentage of users interested",title = "Popular policy areas by cluster")+
  facet_wrap(~cluster)

  
engagement_behavior_graph<- engagement_behavior %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  mutate(engagement = recode(engagement, "quote_count" = "quote","reply_count" = "reply","retweet_count" = "retweet")) %>% 
  ggplot(aes(x = cluster,y = average_engagement))+
  geom_bar(aes(fill = engagement),stat = "identity",position = "dodge")+
  theme_bw()+
  labs(x = "Cluster ID", y = "Average engagement count", title = "Frequency of different engagements by clusters")


p <- (polint_breadt_graph|engagement_behavior_graph)/polint_debth_graph


# Profile content exam ----------------------------------------------------

###need to extract either user names or full dataset on them.