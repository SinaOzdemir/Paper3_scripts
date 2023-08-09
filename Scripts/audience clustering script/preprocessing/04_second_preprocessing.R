# Clustering data preprocessing:


# packages ----------------------------------------------------------------


library(pacman)

p_load(char = c("tidyverse","here"))



# directories -------------------------------------------------------------


data_folder = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/audience_clustering_data"

data_dirs = list.files(path = data_folder, pattern = "*.RDS|*.txt",full.names = T) %>% 
  grep(x = ., pattern = "language",perl = T, value = T, invert = T)


# datasets ----------------------------------------------------------------

p_area_info<- readxl::read_excel(path = here("Data","general_eu_accounts_information.xlsx"),sheet = 1) %>% 
  select(screen_name,Agriculture_Fisheries:Justice_Home_affairs) %>% 
  pivot_longer(cols = Agriculture_Fisheries:Justice_Home_affairs,names_to = "policy_area",values_to = "responsible") %>% 
  group_by(policy_area) %>% 
  summarise(total_actors = sum(responsible))

policy_area = readRDS(data_dirs[1]) %>% 
  mutate(user_id = paste0("x_",user_id)) %>% 
  select(user_id,Agriculture_Fisheries:total_pol_res) %>% 
  pivot_longer(Agriculture_Fisheries:total_pol_res,names_to = "policy_area",values_to = "interest") %>% 
  group_by(user_id,policy_area) %>% 
  summarise(total_interest = sum(interest)) %>% 
  left_join(.,p_area_info,by = "policy_area") %>% 
  mutate(total_actors = replace_na(total_actors,113)) %>% 
  mutate(total_interest = round((total_interest/total_actors),2)) %>% 
  select(-total_actors) %>% 
  pivot_wider(id_cols = user_id,names_from = policy_area,values_from = total_interest)
  

  
engagement = readRDS(data_dirs[5])

polarea_engagement = left_join(policy_area,engagement,by = "user_id") %>% 
  mutate(across(quote_count:retweet_count,~replace_na(.x,0))) %>% 
  ungroup() %>% 
  mutate(across(quote_count:retweet_count,~(.x/max(.x))))

#there are duplicated users in the embeddings file
user_embeddings = read.table(file = data_dirs[7],header = T,sep = ",",stringsAsFactors = F,encoding = "UTF-8") %>% 
  filter(user_id != "user_id") %>% 
  mutate(user_id = str_remove_all(user_id,"\"")) %>% 
  mutate(user_id = paste0("x_",user_id)) %>% 
  distinct(user_id,.keep_all = T)


clustering_data = left_join(polarea_engagement,user_embeddings, by = "user_id") %>% 
  distinct(user_id,.keep_all = T) %>% 
  mutate(across(X0:X99,~replace_na(.x,0)))

saveRDS(clustering_data,file = paste0(data_folder,"/clustering_data_pommed.RDS"))

# scaling the clustering data ---------------------------------------------

clustering_data_scaled<- clustering_data %>% 
  column_to_rownames("user_id") %>% 
  as.matrix.data.frame(rownames.force = T) %>% 
  scale() %>% 
  as.data.frame()

saveRDS(clustering_data_scaled, file = paste0(data_folder,"/clustering_df_scaled.RDS"))
