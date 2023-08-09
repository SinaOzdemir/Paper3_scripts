#Data output test


# setup -------------------------------------------------------------------

require(pacman)

packs<- c("tidyverse","here","feather")

p_load(char = packs)

network_path<- here("Data","L1_network_data")

diffusion_path<- here("Data","Diffusion_data")



# following network -------------------------------------------------------


#loop produced followers data

lp_flw<- readRDS(file = file.path(network_path,"L1_follower_network.RDS"))

glimpse(lp_flw)

flw_dist<- lp_flw %>%
  group_by(user_id) %>% 
  summarise(interest_div = n()) %>% 
  mutate(engaged = 0)

flw_spec_dist<-flw_dist %>%
  ggplot(aes(x = interest_div))+
  geom_density(fill = "#7faf13",color ="#7faf13", alpha = 0.7)+
  theme_bw()+
  labs(x= "number of EU accounts followed",title = "Density plot of audience specialization")

ggsave(flw_spec_dist,filename = "follower_specialization_dis_a.jpg",path = here("Graphs"),bg = "white",width = 5,height = 5,units = "in")

remove(lp_flw,flw_dist,flw_spec_dist)

#loop produced engagers data

total<- readRDS(file = file.path(network_path,"L1_eng_network_total.RDS")) %>% select(-created_at)

glimpse(total)

eng_dist<- total %>% 
  group_by(engager_id) %>% 
  summarise(interest_div = n()) %>% 
  rename(user_id = engager_id) %>% 
  mutate(engaged = 1)

eng_spec_dist<- eng_dist %>%
  ggplot(aes(x = interest_div))+
  geom_density(fill = "#b01616",color = "#b01616",alpha = .7)+
  theme_bw()+
  labs(x = "number of EU accounts followed",title = "Density plot of engaging user interest specialization")


ggsave(eng_spec_dist,filename = "follower_specialization_dis_a.jpg",path = here("Graphs"),bg = "white",width = 5,height = 5,units = "in")

#joint_dist

joint_dist<- rbind(flw_dist,eng_dist)

joint_dist_graph<- joint_dist %>% 
  ggplot(aes(x = interest_div, group = as.factor(engaged),fill = as.factor(engaged)))+
  geom_density(adjust = 2, alpha = .4)+
  theme_bw()+
  labs(x = "number of EU accounts followed",title = "Distribution of audience interest diversity")


ggsave(joint_dist_graph,filename = "audience_interest_joint_dist.jpg",path = here("Graphs"),bg = "white",width = 5,height = 5,units = "in")


###batched eng network

lp_eng<- list.files(path = network_path,pattern = "L1_eng_*",full.names = T) %>%
  grep(pattern = "*.RDS|*.rds",value = T) %>%grep(pattern = "total",value = T,invert = T)


eng_int_dta<- map_dfr(.x = lp_eng,.f = readRDS)

eng_int_dta<- eng_int_dta %>% select(-created_at)


total$match<- ifelse(total$engager_id%in%eng_int_dta$engager_id,T,F)

sum(total$match)

#there seems to be some duplication in engager interest specialization batch data and sum data!

#feather files

eng_network_feather<- read_feather(path = file.path(network_path,"L1_eng_network.feather")) %>% select(-created_at)

flw_network_feather<- read_feather(path = file.path(network_path,"L1_follower_network.feather"))

#so everything has a different value.... fantastic...
# TODO: recreate interest diversity networks!
remove(eng_int_dta,eng_network_feather,lp_flw,total,flw_network_feather)

# diffusion distributions -------------------------------------------------

diff_flw<- list.files(path = diffusion_path, pattern = "*.feather",full.names = T) %>% grep(pattern = "flw",value=T)

diff_flw_dta<- map_dfr(diff_flw,read_feather)

diff_flw_dta<- diff_flw_dta %>% 
  mutate(engaged = F)

diff_eng<- list.files(path = diffusion_path, pattern = "*.feather",full.names = T) %>% grep(pattern="eng",value = T)

diff_eng_dta<- map_dfr(diff_eng,read_feather)

diff_eng_dta<- diff_eng_dta %>% 
  mutate(engaged = T)

diff_dta<- rbind(diff_flw_dta,diff_eng_dta)

test<- diff_dta %>% group_by(user_id,engaged) %>% summarize(diffusion_rate = sum(diffusion_rate))


a<-test %>% ggplot(aes(x = diffusion_rate, group = as.factor(engaged), fill = engaged))+
  geom_density(adjust = 2, alpha = .4)+
  theme_bw()+
  labs(x = "Diffusion rate",title = "Distribution of message diffusion audience")

ggsave(a,filename = "diffusion_rate_distribution.jpg",path = here("Graphs"),bg="white",height = 5,width = 5,units = "in")

#multiplying by follower count make the distribution L shaped


test_b<- diff_dta %>% group_by(user_id,engaged) %>% summarise(eng_rate = sum(engagement_rate))

b<- test_b %>% ggplot(aes(x = eng_rate, group = as.factor(engaged), fill = engaged))+
  geom_density(adjust = 2, alpha = .4)+
  theme_bw()+
  labs(x = "Engagement_rate",title = "Distribution of audience's engagement rate")

ggsave(b,filename = "eng_rate_distribution.jpg",path = here("Graphs"),bg="white",height = 5,width = 5,units = "in")
