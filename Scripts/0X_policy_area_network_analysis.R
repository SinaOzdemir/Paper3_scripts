# policy area network graph:
require(pacman)

packs<- c("tidyverse","here","feather","ggraph","RColorBrewer","igraph","ggraph","xlsx")

p_load(char = packs)

eu_info<- read.xlsx(file = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/general_eu_accounts_information.xlsx",sheetIndex = 1) %>% 
  mutate(user_id = gsub("x_","",.$user_id))


eu_policy<- eu_info %>% 
  select(user_id,name,Agriculture_Fisheries:total_pol_res)

# policy area network graph -----------------------------------------------

#to do: 
##1) include meta information policy areas 
### - how many accounts are there in policy area (this will be node size)
### - add unique color code for each policy area (each account should obtain the same color as well)
#### example: https://r-graph-gallery.com/339-circular-dendrogram-with-ggraph.html


policy_area_network<- eu_policy %>% 
  select(-user_id,-total_pol_res) %>%
  filter(!(name%in%c("European Commission","Ursula von der Leyen","EU Council Press"))) %>% 
  pivot_longer(cols = Agriculture_Fisheries:Justice_Home_affairs,
               names_to = "policy_area",
               values_to = "responsible") %>% 
  filter(responsible == 1) %>% 
  select(policy_area,name) %>% 
  mutate(name = recode(name,
                       "European Commission"  = "EU Commission/Commission President/EU Council",
                       "Ursula von der Leyen" = "EU Commission/Commission President/EU Council",
                       "EU Council Press" = "EU Commission/Commission President/EU Council")) %>% 
  rename(from = policy_area,
         to = name) %>% 
  mutate(from = recode(from,
                       "Agriculture_Fisheries"= "Agriculture and Fisheries",
                       "Education_youth" = "Education and Youth",
                       "General_Affairs" = "General Affairs",
                       "Digit_trans" = "Transportaion, Energy and digitalization",
                       "Economic.and.Financial.affairs" = "Economic and Financial Affairs",
                       "Foreign.Affairs" = "Foreing Affairs",
                       "ESHC"  ="Employment, Social and Healthcare",
                       "Justice_Home_affairs" = "Justice and Home Affairs"))



og_pol_Area<- data.frame(from = rep("EU Commission/Commission President/EU Council",times = length(unique(policy_area_network$from))),
                         to = unique(policy_area_network$from))

policy_area_edges<- rbind(og_pol_Area,policy_area_network)

#EU Commission/Commission President/EU Council
actor_information<- eu_info %>% 
  select(name,total_pol_res,Actor_type) %>% 
  mutate(total_pol_res = 0) %>% 
  distinct(name,.keep_all = T)
  
policy_area_information <- eu_info %>% filter(in_paper_3 == T) %>% 
  select(user_id,Agriculture_Fisheries:Justice_Home_affairs) %>%
  pivot_longer(cols = Agriculture_Fisheries:Justice_Home_affairs,
               names_to = "policy_area",values_to = "responsible") %>% 
  group_by(policy_area) %>% 
  summarise(responsible_count = sum(responsible),
            responsible_perc = round(responsible_count/nrow(filter(eu_info,in_paper_3 == T)),2)) %>% 
  mutate(Actor_type = "Policy Area") %>% 
  rename(total_pol_res =responsible_perc,
         name = policy_area) %>% 
  select(-responsible_count) %>% 
  mutate(name = recode(name,
                       "Agriculture_Fisheries"= "Agriculture and Fisheries",
                       "Education_youth" = "Education and Youth",
                       "General_Affairs" = "General Affairs",
                       "Digit_trans" = "Transportaion, Energy and digitalization",
                       "Economic.and.Financial.affairs" = "Economic and Financial Affairs",
                       "Foreign.Affairs" = "Foreing Affairs",
                       "ESHC"  ="Employment, Social and Healthcare",
                       "Justice_Home_affairs" = "Justice and Home Affairs"))
  
  
policy_network_vertices<- rbind(actor_information,policy_area_information)


political_leadership <- data.frame(name = "EU Commission/Commission President/EU Council",
                                   total_pol_res = 0,
                                   Actor_type = "Political Leadership")

policy_network_vertices<- rbind(policy_network_vertices,political_leadership)

policy_network_vertices<- policy_network_vertices %>% 
  mutate(Actor_type = as.factor(Actor_type))

policy_network_vertices<- with(policy_network_vertices, policy_network_vertices[order(Actor_type,decreasing = T),]) %>% 
  filter(!(name%in%c("Ursula von der Leyen","EU Council Press","European Commission")))


#combine EU_commision/Commisison Presiden/EU Council, in the vertice list
edge_list<- unique(c(policy_area_edges$from,policy_area_edges$to))
vertex_list<- unique(c(policy_network_vertices$name))

missing_vertex_info<- vertex_list[!(vertex_list%in%edge_list)]
missing_edge_info<- edge_list[!(edge_list%in%vertex_list)]


policy_area_igraph<- igraph::graph_from_data_frame(d = policy_area_edges,vertices = policy_network_vertices,
                                                   directed = T)


## Graph:


policy_area_igraph %>% 
  ggraph(layout = "dendrogram",circular = T)+
  geom_edge_diagonal(colour = "grey")+
  geom_node_point(aes(color = Actor_type, size = (total_pol_res*10)))+
  geom_node_text(aes(x = x*1.20,
                     y=y*1.20,
                     label = name,
                     angle = -((-node_angle(x, y)+90)%%180)+90,
                     color = Actor_type),show.legend = F)+
  guides(color = guide_legend(title = "Actor Type"))+
  theme_void()+
  guides(size = "none")
  
#TODO:
## Color coding of nodes based on actor type does not assign correct colors
## When node size specified, text should be nudged(hjust) otherwise point covers the text