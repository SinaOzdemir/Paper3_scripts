# cluster extraction


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse"))



# paths -------------------------------------------------------------------


results_dir = "M:/cluster count/results"


# hierarchical clusters ---------------------------------------------------

#1) average distance

avg_dist = readRDS(file = paste0(results_dir,"/haverage_n2_results.RDS"))

avg_hclust_user = data.frame()
for (i in 1:length(avg_dist)) {
  clust = cutree(avg_dist[[i]], k = 2)
  clust_i = data.frame(user_id = names(clust),
                       cluster = clust)
  avg_hclust_user = rbind(avg_hclust_user,clust_i)
}
saveRDS(object = avg_hclust_user,file = paste0(results_dir,"/avg_hclust_users.RDS"))

#2) complete distance

comp_dist = readRDS(file = paste0(results_dir,"/hcomplete_n2_results.RDS"))

comp_hclust_user = data.frame()
for (i in 1:length(comp_dist)) {
  clust = cutree(comp_dist[[i]], k = 2)
  clust_i = data.frame(user_id = names(clust),
                       cluster = clust)
  comp_hclust_user = rbind(comp_hclust_user,clust_i)
}
saveRDS(object = comp_hclust_user,file = paste0(results_dir,"/comp_hclust_users.RDS"))

# 3) single distance

sing_dist = readRDS(file = paste0(results_dir,"/hsingle_n2_results.RDS"))

sing_hclust_user = data.frame()
for (i in 1:length(sing_dist)) {
  clust = cutree(sing_dist[[i]], k = 2)
  clust_i = data.frame(user_id = names(clust),
                       cluster = clust)
  sing_hclust_user = rbind(sing_hclust_user,clust_i)
}
saveRDS(object = sing_hclust_user,file = paste0(results_dir,"/sing_hclust_users.RDS"))


# 4) ward distance

ward_dist = readRDS(file = paste0(results_dir,"/hward_n2_results.RDS"))

ward_hclust_user = data.frame()
for (i in 1:length(ward_dist)) {
  clust = cutree(ward_dist[[i]], k = 2)
  clust_i = data.frame(user_id = names(clust),
                       cluster = clust)
  ward_hclust_user = rbind(ward_hclust_user,clust_i)
}
saveRDS(object = ward_hclust_user,file = paste0(results_dir,"/ward_hclust_users.RDS"))


#combined

hclust_users = avg_hclust_user %>% 
  rename(avg_cluster = cluster) %>% 
  mutate(comp_cluster = comp_hclust_user$cluster,
         single_cluster = sing_hclust_user$cluster,
         ward_cluster = ward_hclust_user$cluster)

saveRDS(hclust_users, file = paste0(results_dir,"/hclust_users.RDS"))
