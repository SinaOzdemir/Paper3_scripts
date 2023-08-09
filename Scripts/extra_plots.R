# account similarities based on common followers and policy area responsbilities


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","feather","here","igraph","ggraph"))


# similarity based on policy area -----------------------------------------

eu_data<- readxl::read_excel(path = here("Data","general_eu_accounts_information.xlsx"),sheet = 1)

similarity_data<- eu_data %>% select(screen_name_l,Agriculture_Fisheries:Justice_Home_affairs) %>% 
  column_to_rownames(var = "screen_name_l") %>% as.matrix.data.frame()
#percentage agreement_style is better
policy_jaccard_sim<- as.matrix(1- dist(similarity_data,method = "binary",upper = T)) %>% as.data.frame()

policy_sim_long<- policy_jaccard_sim %>% 
  rownames_to_column(var = "from") %>% 
  pivot_longer(cols = adinavalean:ylvajohansson,names_to = "to",values_to = "similarity") %>% 
  filter(from != to)



#audience based similarity

#rows are audience, cols are EU
audience_data<-read_feather(path = here("Data","L1_network_data","EU_audience_commonality.feather"))

eu_names<- eu_data %>% select(user_id,name) %>% mutate(user_id = gsub("x_","",user_id))

recoding_key <- eu_names$name

names(recoding_key)<- eu_names$user_id

audience_data<- audience_data %>% 
  mutate(eu_a = recode(eu_a,!!!recoding_key)) %>% 
  mutate(eu_b = recode(eu_b,!!!recoding_key))


cor_cal<- left_join(audience_data,policy_sim_long,by = c("eu_a" = "from","eu_b" = "to")) %>% drop_na()


correlation<- cor(cor_cal$weight,cor_cal$similarity)


# jaccard for audience commonality test

audience<-read_feather(path = here("Data","L1_network_data","L1_wide_network_data.feather"))

audience_t<- t(audience)

similarity<- 1-dist(audience_t,method = "binary")


audience_similarity_df<- similarity %>% as.matrix() %>% as.data.frame() %>% 
  rownames_to_column(var = "from") %>% 
  pivot_longer(cols = `2476130260`:`1148530793171607553`,names_to = "to",values_to = "weight")

eu_names<- eu_data %>% select(user_id,screen_name) %>% mutate(user_id = gsub("x_","",user_id))


recoding_key <- eu_names$screen_name

names(recoding_key)<- eu_names$user_id

audience_similarity_df<- audience_similarity_df %>% 
  mutate(from = recode(from,!!!recoding_key)) %>% 
  mutate(to = recode(to,!!!recoding_key)) %>% 
  filter(from%in%eu_names$screen_name) %>% 
  filter(to%in%eu_names$screen_name)

#clean up
audience_similarity_df<- audience_similarity_df %>% 
  filter(from != to)

vertices<- unique(audience_similarity_df$from)
edges<- audience_similarity_df %>% select(weight) %>% mutate(weight = as.numeric(weight))

network_graph<- graph_from_data_frame(audience_similarity_df,directed = F,vertices = vertices)

lower_limit_oneprc<-min(slice_max(edges,order_by = weight,prop = .01)$weight)

s1<- subgraph.edges(graph = network_graph,eids = E(network_graph)[E(network_graph)$weight>=lower_limit_oneprc],delete.vertices = T)


network_plot<- s1 %>% ggraph(layout="linear",circular = T)+
  geom_node_point(color = "steelblue")+
  geom_node_text(aes(x = x*1.1, y=y*1.1,label = names(V(s1)),angle = -((-node_angle(x, y)+90)%%180)+90),size = 3,fontface = "bold")+
  geom_edge_arc(aes(edge_alpha = E(s1)$weight),edge_width = 2,lineend = "round",start_cap = circle(.5,"cm"),end_cap = circle(.5,"cm"))+
  theme_graph(background = "white")+
  guides(edge_alpha = guide_legend(title = "Jaccard similarity of audience"))+
  labs(caption = "top 1% of the audience homogeneity")


network_plot+geom_node_text(aes(x = x*1.15, y=y*1.15,label = names(V(s1)),angle = -((-node_angle(x, y)+90)%%180)+90))+
  
#for shits and giggles

ggplot(audience_similarity_df,aes(x= from,y= to ,fill = weight))+
  geom_tile()+
  theme_bw()

similarities<- left_join(audience_similarity_df,policy_sim_long,by = c("from","to")) %>% drop_na()

cor(similarities$weight,similarities$similarity)

#more shits and giggles

sg<- audience_similarity_df %>% distinct(weight,.keep_all = T)

top_10<- sg %>% slice_max(order_by = weight,n = 10)
bottom_10<- sg %>% slice_min(order_by =weight,n = 10)

top_bottom<- rbind(top_10,bottom_10) %>% 
  mutate(weight = round(weight,2)) %>% 
  mutate(dyads = paste0(to," - ",from)) %>% 
  ggplot(aes(x = reorder(dyads,weight), y = weight))+
  geom_bar(stat = "identity",position = "dodge",fill = "steelblue")+
  theme_bw()+
  labs(x = "EU executive account", y = "Jaccard similarity index")+
  coord_flip()+
  scale_y_continuous(breaks = seq(0,1,.1))

#studied eu

studied_eu<- eu_data %>% mutate(user_id = gsub("x_","",user_id)) %>% filter(user_id%in%eu_actors)

appendix_data<- studied_eu %>% select(user_id,screen_name,name,followers_count) %>% mutate(screen_name = paste0("@",screen_name))

library(flextable)

appendix_data %>% qflextable() %>% flextable::save_as_docx(path = here("Drafts","appendix_table.docx"))
