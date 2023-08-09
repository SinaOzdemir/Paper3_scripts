####################################################################
# Title:  Network analysis of audience homogeneity of L0           #
# Author: Sina Özdemir                                             #
#         PhD candidate                                            #
#         Department of sociology and political science            #
#         sina.ozdemir@ntnu.no                                     #
####################################################################

# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","igraph","ggraph","here","feather")

p_load(char = packs)


# data --------------------------------------------------------------------

raw_network<- read_feather(path = here("Data","L1_network_data","L1_wide_network_data.feather"))


# make audience homogeneity dataset ---------------------------------------


eu_accounts<- colnames(raw_network)

eu_account_comb <- combn(eu_accounts,2, simplify = F)


eu_audience_share <- data.frame()

for (i in 1:length(eu_account_comb)) {
  cat("calculating combination: ",i)
  
  pair<-eu_account_comb[[i]]
  
  audience_network<- network_data[,pair]
  
  common_audience<- sum(ifelse(audience_network[,1]== 1 & audience_network[,2]==1,1,0))
  
  audience_based_network<- data.frame(eu_a = pair[1],
                                      eu_b = pair[2],
                                      weight = common_audience)
  
  eu_audience_share<- rbind(eu_audience_share,audience_based_network)
  
  rm(pair,audience_network,common_audience,audience_based_network)
  
}

write_feather(x = eu_audience_share,path = here("Data","L1_network_data","EU_audience_commonality.feather"))




# network data cleaning ---------------------------------------------------


ac_data<-read_feather(path = here("Data","L1_network_data","EU_audience_commonality.feather"))

eu_profiles_dta<- readRDS(file = here("Data","eu_profile.RDS")) %>%
  filter(!duplicated(user_id)) %>% 
  select(user_id,screen_name)

actor_info<-read.csv(file = here("Data","analysis_accounts_inf_vars.csv")) %>% 
  select(screen_name,Actor_type)

eu_info<- left_join(eu_profiles_dta,actor_info, by = "screen_name")
# vertex info test --------------------------------------------------------

missing_vertex_info<- eu_profiles_dta %>% filter(!(screen_name%in%actor_info$screen_name))
#most important here is nicolas schmidt, the rest is not that important

duplicated_information<- eu_info %>% filter(duplicated(user_id))

missing_actor_type <- eu_info %>% filter(is.na(Actor_type))


# vertex information correction -------------------------------------------
# duplicated information--------------

duplicate_correction<- eu_info %>%filter(!duplicated(user_id))

duplicate_correction[(duplicate_correction$user_id == "193301414"& duplicate_correction$screen_name == "EUScienceInnov"),"Actor_type"]<-"Directorate general"

duplicate_correction[(duplicate_correction$user_id == "544312946"& duplicate_correction$screen_name == "nicolasschmit2"),"Actor_type"]<- "Commissioner"  

duplicate_correction[(duplicate_correction$user_id == "1343714233"& duplicate_correction$screen_name == "EUCourtPress"),"Actor_type"]<-"Institution"

duplicate_correction<- duplicate_correction %>% 
  mutate(Actor_type = recode(Actor_type, "High representative and vice president" = "High rep."))

#identify which of the vertex ids are missing in the information df

vertex_id<-c(unique(ac_data$eu_a),unique(ac_data$eu_b))
vertex_id<- vertex_id[!duplicated(vertex_id)]

#missing vertex information

still_missing<- duplicate_correction %>% filter((user_id%in%vertex_id))
#phill hogan (commissioner for trade between 2019-2020)is absent from the network data
#this should not be a major problem since he is not a part of VdL commission

#Actor_type == NA

a_type_missing<- duplicate_correction %>% filter(!(user_id == "232242473" & screen_name == "PhilHoganEU")) %>% filter(is.na(Actor_type))
#Europarl_EN
#EU_EESC
#EU_CoR
#These are not executives, but advisory/legislative bodies.They are probably an artifact of earlier phd dataset

#are they present in the network data?

nexec_network<- ac_data %>% filter(eu_a%in%a_type_missing$user_id)
#yep they are, should remove them from both network and vertex info datasets

ac_data<- ac_data %>% 
  filter(!(eu_a%in%a_type_missing$user_id)) %>% 
  filter(!(eu_b%in%a_type_missing$user_id))

vertex_info<- duplicate_correction %>%
  filter(!(user_id == "232242473" & screen_name == "PhilHoganEU")) %>%
  filter(!is.na(Actor_type))
#everything should be cleaned now

vertex_info_c<- vertex_info %>% pull(user_id)

vertex_id<- c(ac_data$eu_a,ac_data$eu_b) %>% unique()

a<- vertex_info_c[!(vertex_info_c%in%vertex_id)]
#alright now vertices in network dataset and info dataset match now but there is now 114 accounts
#should identify which ones are missing.


og_network<- colnames(read_feather(path = here("Data","L1_network_data","L1_wide_network_data.feather")))

extra_users<- og_network[!(og_network%in%vertex_info_c)]
#these are again Europarl_EN,EU_EESC,EU_CoR which shouldn't have been in the dataset to begin with...

#TODO: reproduce the code to create and save the clean igraph object


# Network analysis sandbox ------------------------------------------------


audience<-read_feather(path = here("Data","L1_network_data","L1_wide_network_data.feather"))

audience_t<- t(audience)

similarity<- 1-dist(audience_t,method = "binary")


audience_similarity_df<- similarity %>% as.matrix() %>% as.data.frame() %>% 
  rownames_to_column(var = "from") %>% 
  pivot_longer(cols = `2476130260`:`1148530793171607553`,names_to = "to",values_to = "weight")

eu_names<- eu_data %>% select(user_id,screen_name_l) %>% mutate(user_id = gsub("x_","",user_id))

recoding_key <- eu_names$screen_name_l

names(recoding_key)<- eu_names$user_id

audience_similarity_df<- audience_similarity_df %>% 
  mutate(from = recode(from,!!!recoding_key)) %>% 
  mutate(to = recode(to,!!!recoding_key)) %>% 
  filter(from%in%eu_names$screen_name_l) %>% 
  filter(to%in%eu_names$screen_name_l)

#clean up
audience_similarity_df<- audience_similarity_df %>% 
  filter(from != to)

vertices<- unique(audience_similarity_df$from)
edges<- audience_similarity_df %>% select(weight) %>% mutate(weight = as.numeric(weight))

network_graph<- graph_from_data_frame(audience_similarity_df,directed = F,vertices = vertices)

lower_limit_oneprc<-min(slice_max(edges,order_by = weight,prop = .01)$weight)

s1<- subgraph.edges(graph = network_graph,eids = E(network_graph)[E(network_graph)$weight>=.3],delete.vertices = F)


network_plot<- s1 %>% ggraph(layout="linear",circular = T)+
  geom_node_point(color = "steelblue")+
  geom_edge_arc(aes(edge_alpha = E(s1)$weight),edge_width = 2,lineend = "round",start_cap = circle(.5,"cm"),end_cap = circle(.5,"cm"))+
  theme_graph(background = "white")

network_plot+geom_node_text(aes(x = x*1.15, y=y*1.15,label = names(V(s1)),angle = -((-node_angle(x, y)+90)%%180)+90))+
  guides(edge_alpha = guide_legend(title = "Jaccard similarity of audience"))

# garbage -----------------------------------------------------------------

    
network_a<- edges %>% filter(weight<edge_quantiles[2]) %>% mutate(node_count = length(unique(c(.$from))))
network_b<- edges %>% filter(weight>=edge_quantiles[2]& weight<edge_quantiles[3])%>% mutate(node_count = length(unique(c(.$from))))
network_c<- edges %>% filter(weight>=edge_quantiles[3]&weight<edge_quantiles[4])%>% mutate(node_count = length(unique(c(.$from))))
network_d<- edges %>% filter(weight >= edge_quantiles[4]& weight <= edge_quantiles[5])%>% mutate(node_count = length(unique(c(.$from))))

##network test

network_d_vertices<- vertices %>% filter(name%in%network_d$from | name%in%network_d$to)

network_d_graph<- network_d %>% select(-node_count) %>% graph_from_data_frame(d = .,directed = F,vertices = network_d_vertices)

E(network_d_graph)$weight<- E(network_d_graph)$weight*.0001

quantile(E(network_d_graph)$weight)

E(network_d_graph)$weight<-ifelse(E(network_d_graph)$weight<.43,0,1)


ggraph(network_d_graph,layout = "circle")+
  geom_node_point(aes(size =15, color = as.factor(V(network_d_graph)$Actor_type)))+
  geom_node_text(aes(label = V(network_d_graph)$screen_name))+
  geom_edge_link(aes(alpha = (E(network_d_graph)$weight)))+
  theme_graph(background = "white")

#so this works and gives me what I want
s1<- subgraph.edges(network_d_graph,E(network_d_graph)[E(network_d_graph)$weight>=19454],delete.vertices = T)

plot(s1,
     layout= layout_with_fr(graph = s1),
     edge.width = (E(s1)$weight*.0001),
     edge.color="grey50",
     vertex.label = paste0(V(s1)$screen_name,"(",V(s1)$Actor_type,")"))
title(main = list("Audience commonality (edge weight>19000)",
                  cex=1,col="black"))



