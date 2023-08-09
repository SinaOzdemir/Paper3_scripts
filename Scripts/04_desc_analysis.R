####################################################################
# Title:  Descriptive analysis of L1 network                       #
# Author: Sina Özdemir                                             #
#         PhD candidate                                            #
#         Department of sociology and political science            #
#         sina.ozdemir@ntnu.no                                     #
####################################################################


# setup -------------------------------------------------------------------

require(pacman)

packs<- c("tidyverse","here","feather","treemapify","patchwork","igraph","ggraph")

p_load(char = packs)

l1_ntw_dt<- list.files(path = here("Data","L1_network_data"),pattern = "*_eu.feather",full.names = T) %>% 
  map_dfr(.,read_feather)

#Some user-eu dyads are duplicated for some reason
eu_profiles_dta<-readxl::read_excel(path = here("Data",
                                                "EU_account_information",
                                                "general_eu_accounts_information.xlsx"),
                                    sheet = 1)%>%
  filter(!duplicated(user_id))


data_integrity<- l1_ntw_dt %>% distinct(eu_accounts,user_id)
  
data_integrity<- eu_profiles_dta %>%
  mutate(user_id = gsub("x_","",user_id)) %>% 
  rename(eu_accounts = user_id) %>% 
  left_join(data_integrity,.,by = "eu_accounts") %>% 
  select(-in_paper_1,-in_paper_2,-in_paper_3,-reason_p2) %>% 
  drop_na()



# interest diversity ------------------------------------------------------

interest_diversity<- data_integrity %>% 
  select(user_id, Agriculture_Fisheries:Justice_Home_affairs) %>% 
  group_by(user_id) %>% 
  summarise(Agriculture_and_Fisheries = sum(Agriculture_Fisheries),
            Competition = sum(Competetiveness),
            D_Transformation = sum(Digit_trans),
            Economy_and_Financial_Affairs = sum(Economic.and.Financial.affairs),
            Education_and_Youth = sum(Education_youth),
            Environment = sum(Environment),
            ESHC = sum(ESHC),
            Foreign_Affairs = sum(Foreign.Affairs),
            General_Affairs = sum(General_Affairs),
            Justice_and_Home_Affairs = sum(Justice_Home_affairs)) %>% 
  ungroup() %>% 
  mutate(user_id = paste0("x_",user_id))

interest_breadth<- interest_diversity %>% 
  mutate(across(Agriculture_and_Fisheries:Justice_and_Home_Affairs,~ifelse(.x>0,1,0))) %>% 
  reframe(p_interest_breath = Agriculture_and_Fisheries+Competition+D_Transformation+Economy_and_Financial_Affairs+Education_and_Youth+Environment+ESHC+Foreign_Affairs+General_Affairs+Justice_and_Home_Affairs,.by = "user_id")

policy_interest<- left_join(interest_diversity,interest_breadth,by = "user_id")


saveRDS(data_integrity,file = here("Data","results_data","audience_policy_area_interest.RDS"))

int_quantile<- quantile(int_div$int_div,names = T)

f_quant<- int_div %>% 
  filter(int_div >6 & int_div<111)

f_q_q<- quantile(f_quant$int_div)

int_div<- int_div %>% 
  mutate(div_class = ifelse(int_div <=3,"minimal(n<3)",
                            ifelse(int_div >3 & int_div<=6,"somewhat(3<n<7)",
                                   ifelse(int_div>6 & int_div<=19,"high(6<n<20)",
                                          ifelse(int_div>19,"extreme(n>19)",NA)))))


#add labels to partition lines
int_div_dist<- int_div %>% 
  ggplot(aes(x= int_div))+
  geom_density(fill = "#BADA55",color = "#4A5722")+
  geom_vline(xintercept = 3,color = "red")+
  annotate(geom = "text",x = 2,y=.37,label = "minimal(n<3)",color = "red",angle = 90,size = 5)+
  geom_vline(xintercept = 6,color = "red")+
  annotate(geom = "text",x = 5,y=.37,label = "somewhat(3<n<7)",color = "red",angle = 90,size = 5)+
  geom_vline(xintercept = 19,color = "red")+
  annotate(geom = "text",x = 18,y=.37,label = "high(6<n<20)",color = "red",angle = 90,size = 5)+
  geom_vline(xintercept = 111,color = "red")+
  annotate(geom = "text",x = 110,y=.37,label = "extreme(n>19)",color = "red",angle = 90,size = 5)+
  theme_bw()+
  labs(x = "N of followed EU executives")

ggsave(filename = "interest_diversity_dist.jpg",plot = int_div_dist,path = here("Graphs"),bg = "white")


#(DON'T RUN) audience diversity by source origin -------------------------------
audience_source_files<- list.files(path = here("Data","EU_followers","follower_ids"),pattern = "*.RDS",full.names = T)

audience_source<- list.files(path = here("Data","EU_followers","follower_ids"),pattern = "*.RDS",full.names = F) %>% 
  gsub("followers_|.RDS","",x = .)

eu_account_dist<- data.frame()
for (i in 1:length(audience_source_files)) {
  eu_account_followers<- readRDS(audience_source_files[i]) %>% 
    mutate(source_account = audience_source[i])
  
  eu_account_followers_int<- inner_join(eu_account_followers,int_div, by = "user_id")
  
  eu_account_dist<- rbind(eu_account_dist,eu_account_followers_int)
  
  remove(eu_account_followers,eu_account_followers_int)
}

eu_account_dist_a<-inner_join(eu_account_dist,eu_info,by = c("source_account"="eu_accounts"))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


md_int_div<- eu_account_dist_a %>% 
  group_by(Actor_type) %>% 
  summarise(mode_interest = getmode(int_div))

mode_int_graph<- md_int_div %>% 
  ggplot(aes(x = reorder(Actor_type,mode_interest),y = mode_interest))+
  geom_bar(aes(fill = mode_interest), stat = "identity",position = "dodge")+
  theme_bw()+
  coord_flip()+
  guides(fill = guide_legend(title = "Mode value"))+
  labs(x = "Actor type", y = "N of followed eu accounts", title = "Mode of audience interest diversity by EU executive type")


ggsave(mode_int_graph,filename = "mode_audience_int_div.jpg",path = here("Graphs"),bg ="white")

# Diffusive behaviour -----------------------------------------------------
##follower ids:
follower_ids<- read_feather(path = here("Data","L1_network_data","L1_flw_network_eu.feather")) %>% 
  distinct(user_id) %>% pull(user_id)

test = read_feather(path = here::here("Data","Diffusion_data","l1_diffusion_data.feather"))

followers_only<- test %>%
  filter(user_id%in%follower_ids) %>% 
  distinct(user_id,.keep_all = T)


intermediate_followers<- followers_only %>% 
  select(user_id,engagement_rate) %>% 
  mutate(intermediary = ifelse(.$engagement_rate>0,"Intermediary","Final")) %>% 
  group_by(intermediary) %>% 
  tally() %>% 
  mutate(perc = round((n/nrow(followers_only)),2)*100)

intermediate_followers_graph<- intermediate_followers %>% 
  ggplot(aes(x = intermediary, y = perc))+
  geom_bar(stat = "identity", position = "dodge", fill = "steelblue")+
  theme_bw()+
  labs(x = "Audience type",y = "Percent",caption = paste0("N = ",nrow(followers_only)))

intermediary_audience<- test %>% 
  group_by(user_id,engagement_type) %>% 
  summarise(engagement_count = sum(engagement_rate)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = engagement_type,values_from = engagement_count,values_fill = 0) %>% 
  mutate(engagement_sort = ifelse(.$quote_count ==0 & .$retweet_count == 0 & .$reply_count>0,"reply only",
                                  ifelse(.$quote_count ==0 & .$retweet_count >0 & .$reply_count==0, "retweet only",
                                         ifelse(.$quote_count >0 & .$retweet_count == 0 & .$reply_count==0,"quote only",
                                                ifelse(.$quote_count >0 & .$retweet_count >0 & .$reply_count==0,"retweet and quote",
                                                       ifelse(.$quote_count ==0 & .$retweet_count > 0 & .$reply_count>0,"retweet and reply",
                                                              ifelse(.$quote_count >0 & .$retweet_count == 0 & .$reply_count>0, "quote and reply",
                                                                     ifelse(.$quote_count >0 & .$retweet_count > 0 & .$reply_count>0,"all three","No engagement"))))))))

engagement_pref<- intermediary_audience %>% 
  group_by(engagement_sort) %>% 
  tally() %>% 
  mutate(perc = round(n/nrow(a),2)*100) %>% 
  filter(engagement_sort != "No engagement")


eng_pref_graph<- engagement_pref %>% 
  ggplot(aes(x = reorder(engagement_sort,-perc),y = perc))+
  geom_bar(stat = "identity", position = "dodge",fill = "steelblue")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = .5))+
  labs(x = "engagement type",y = "Percentage of intermediary audience",caption = paste0("N = ",nrow(a)))+
  scale_y_continuous(breaks = seq(0,100,5))


intermediate_followers_graph+eng_pref_graph

# trash -------------------------------------------------------------------


diffusion_w = test %>% 
  group_by(user_id,engagement_type) %>% 
  summarise(engagement_rate = sum(engagement_rate)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = engagement_type,values_from = engagement_rate,values_fill = 0) %>% 
  distinct(user_id,.keep_all = T) %>% 
  mutate(user_id = paste0("x_",user_id)) %>% 
  filter(user_id%in%policy_interest$user_id)

saveRDS(diffusion_w,file = here("Data","results_data","engagement_data.RDS"))

diffusion_path<- here("Data","Diffusion_data")

diff_files<- list.files(path = diffusion_path,pattern = "*.feather",full.names = T)

diff_data<- map_dfr(diff_files,.f = read_feather)

diff_data_integ<- diff_data %>%
  distinct(engagement_type,engaged_user,user_id,.keep_all = T) %>% 
  ungroup()

write_feather(x = diff_data_integ,path= here::here("Data","Diffusion_data","l1_diffusion_data.feather"))

retweet_dist<- diff_data_integ %>%
  mutate(engagement_type = str_remove_all(string = engagement_type,pattern = "_count")) %>% 
  filter(engagement_type == "retweet") %>% 
  ggplot(aes(x = diffusion_rate))+
  geom_density()+
  theme_bw()+
  labs(x = "",subtitle = "Engagement type: Retweet")


quote_dist<- diff_data_integ %>%
  mutate(engagement_type = str_remove_all(string = engagement_type,pattern = "_count")) %>% 
  filter(engagement_type == "quote") %>% 
  ggplot(aes(x = diffusion_rate))+
  geom_density()+
  theme_bw()+
  labs(x = "diffusion rate\n(eng rate * follower count)",subtitle = "Engagement type: Quote",y = "")

reply_dist<- diff_data_integ %>%
  mutate(engagement_type = str_remove_all(string = engagement_type,pattern = "_count")) %>% 
  filter(engagement_type == "reply") %>% 
  ggplot(aes(x = diffusion_rate))+
  geom_density()+
  theme_bw()+
  labs(x = "",subtitle = "Engagement type: Reply",y="")


eng_dist<- retweet_dist+quote_dist+reply_dist+plot_annotation(title = "Distribution of diffusion via engagement")

ggsave(filename = "diffusion_dist.jpg",plot = eng_dist,path = here("Graphs"),bg = "white")

# diffusing user by source origin -----------------------------------------

diffusers_by_account<- inner_join(diff_data_integ,eu_info,by = c("engaged_user"="eu_accounts"))

diffusers_by_account<- diffusers_by_account %>% 
  mutate(Actor_type = recode(Actor_type, "High representative and vice president" = "High rep."))
   
  
test<- diffusers_by_account %>%
  ggplot(aes(x = Actor_type,y = diffusion_rate))+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1, vjust = .9))+
  labs(x = "Actor type", y = "diffusion rate", title = "distribution of diffusing users by actor type")

ggsave(test, filename = "diffuser_by_actor_type.jpg",path = here("Graphs"),bg = "white")

rm(list = ls())

# audience commonality ----------------------------------------------------

network_data<- read_feather(path = here("Data","L1_network_data","L1_wide_network_data.feather"))


#test

a<- network_data[,1:2]

a$match = ifelse(a[,1]==1 & a[,2]==1,1,0)

b<- a[(a$match == 1),1:2]

c<- data.frame(eu_a = colnames(b)[1],
               eu_b = colnames(b)[2],
               weight = nrow(b))



#test cross()
library(purrr)
eu_accounts<- colnames(network_data)

test2<- purrr::cross(eu_accounts)
#doesnt really work

#test combn()

test3<- combn(x = eu_accounts,m = 2,simplify = F) 

#this worked like a charm....
#implementation test

a<- test3[[1]]
b<- network_data[,a]
c<- sum(ifelse(b[,1] == 1 & b[,2]== 1,1,0))

#now implement this as a loop but with all possible dyadic combinations

eu_accounts<- colnames(network_data)

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



# network analysis --------------------------------------------------------

library(igraph)
library(ggraph)


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
#duplicated information

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

eu_vertex_info<- duplicate_correction %>%
     filter(!(user_id == "232242473" & screen_name == "PhilHoganEU")) %>%
     filter(!is.na(Actor_type))
# igraph ------------------------------------------------------------------

network_data<- readRDS(here("Data","L1_network_data","EU_audience_commonality_clean_network.RDS"))

eu_vertex_info<- readRDS(here("Data","L1_network_data","EU_account_info.RDS"))

network_data<- igraph::graph_from_data_frame(d = ac_data,directed = T,vertices = eu_vertex_info)

saveRDS(object = network_data,file = here("Data","L1_network_data","EU_audience_commonality_clean_network.RDS"))


#vertex.color = V(network_data)$Actor_type is not recognised as color, need to define a palette 

# igraph::plot.igraph(network_data,
#                     layout = layout_in_circle(graph = network_data),
#                     vertex.label = "",
#                     edge.arrow.size = 0,
#                     edge.width =(E(network_data)$weight*.001),
#                     edge.curved = .5)
a<- igraph::as_data_frame(x = network_data,what = "edges")


ggraph(test_network,layout = "circle")+
  geom_node_point(aes(color = as.factor(V(test_network)$Actor_type)))+
  geom_edge_link(aes(edge_width = E(test_network)$weight,edge_alpha = E(test_network)$weight))+
  theme_graph(background = "white")

tree_map<-a %>%  ggplot(aes(x = from,y = to,area = weight ,fill = weight))+geom_treemap()+theme(axis.text.x = element_text(angle=45))

raster_map<- ggplot(a, aes(x= from, y = to)) +
  geom_raster(aes(fill = weight), interpolate = TRUE)

heat_map<- ggplot(a, aes(x= from, y = to)) +
  geom_tile(aes(fill = weight))


##go back to og network data to reduce it down to 10 types to see who follows whoms or something (this doesn't make any sense...)