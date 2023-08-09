# Graph replotting:


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("here","tidyverse","crosstable","feather"))


# paths -------------------------------------------------------------------

data_path<- here("Data")


# replotting --------------------------------------------------------------

## policy area responsiblity

data<- readxl::read_excel(path = here(data_path,"general_eu_accounts_information.xlsx"),sheet = 1) %>% 
  filter(in_paper_3 == T) %>% 
  select(Actor_type,Agriculture_Fisheries:Justice_Home_affairs)


ct1 <- data %>% 
  crosstable(cols = Agriculture_Fisheries:Justice_Home_affairs,by = Actor_type,total = "row",percent_pattern = "{n}\n({p_col})",percent_digits = 0) %>% 
  mutate(label = recode(label,
                        "Agriculture_Fisheries" = "Agriculture and Fisheries",
                        "Digit_trans" = "Digital Transformation",
                        "Economic.and.Financial.affairs" = "Economic and Financial Affairs",
                        "Education_youth" = "Education and Youth",
                        "ESHC" = "Emploment, Social Policy, Health and Consumer Affairs",
                        "Foreign.Affairs" = "Foreign Affairs",
                        "General_Affairs" = "General Affairs",
                        "Justice_Home_affairs" = "Justice and Home Affairs")) %>%
  filter(variable ==1) %>% 
  select(-variable) %>% 
  as_flextable()

# policy area breadth distribution

rm(list = ls())

user_pi_br<- readRDS(file= here("Data","audience_clustering_data","audience_policy_area_interest.RDS")) 

user_pi_wo_outliers<-user_pi_br%>% 
  filter(!(screen_name_l%in%c("eucopresident","eu_commission","vonderleyen","eucouncilpress"))) %>% 
  select(user_id,Agriculture_Fisheries:Justice_Home_affairs) %>% 
  pivot_longer(cols = Agriculture_Fisheries:Justice_Home_affairs,names_to = "policy_area",values_to = "interested") %>% 
  group_by(user_id,policy_area) %>% 
  summarise(interest_depth = sum(interested)) %>% 
  mutate(is_interested = case_when(interest_depth >0 ~1,.default = 0)) 

user_pi_interest_breadt <- user_pi_wo_outliers %>% 
  group_by(user_id) %>% 
  summarise(interest_breadth = sum(is_interested)) %>% 
  group_by(as.factor(interest_breadth)) %>% 
  tally() %>% 
  mutate(perc = round(n/length(unique(user_pi_wo_outliers$user_id))*100,1))

user_pi_ib_graph<- user_pi_interest_breadt %>% 
  ggplot(aes(x= `as.factor(interest_breadth)`,y = perc))+
  geom_bar(aes(fill = perc),stat = "identity",position = "dodge",show.legend = F)+
  theme_bw()+
  labs(x = "number of policy areas", y = "percentage of audience",caption = paste0("N of users=",length(unique(user_pi_wo_outliers$user_id)),"\n","followers of the Commission, Council and their leadership are excluded"))

library(ggridges)

user_pi_id_graph<- user_pi_wo_outliers %>% 
  ggplot(aes(x = interest_depth, y = policy_area,fill = policy_area))+
  geom_density_ridges(alpha = 0.6, stat="binline", bins=30,scale = .9)+
  theme_ridges()+
  theme(legend.position = "none")+
  labs(x = "Number of accounts from a policy area followed", y = "Policy area")+
  scale_y_discrete(labels=c("Agriculture_Fisheries" = "Agriculture and Fisheries",
                                                         "Digit_trans" = "Digital Transformation",
                                                         "Economic.and.Financial.affairs" = "Economic and Financial Affairs",
                                                         "Education_youth" = "Education and Youth",
                                                         "ESHC" = "Emploment, Social Policy, Health and Consumer Affairs",
                                                         "Foreign.Affairs" = "Foreign Affairs",
                                                         "General_Affairs" = "General Affairs",
                                                         "Justice_Home_affairs" = "Justice and Home Affairs"))+
  scale_x_continuous(breaks = seq(0, 60, 2))

rm(list = ls())
## Engagement distributions:

engagement_data<- read.table(file = here("Data","audience_clustering_data","user_engagements.txt"),header = T,sep = ",",stringsAsFactors = F,fileEncoding = "UTF-8")

eu_data<- readxl::read_excel(path = here("Data","general_eu_accounts_information.xlsx"),sheet = 1) %>% 
  filter(in_paper_3 == T) %>% pull(user_id)

all_users <-readRDS(file= here("Data","audience_clustering_data","audience_policy_area_interest.RDS")) %>%
  distinct(user_id) %>%
  mutate(user_id = paste0("x_","",user_id))
  

retweet_data<- engagement_data %>% 
  filter(retweet_user_id%in%eu_data) %>% 
  group_by(user_id) %>% 
  tally(name = "retweet_count") %>% 
  right_join(.,all_users,by = "user_id") %>% 
  mutate(retweet_count = replace_na(retweet_count,0))

quote_count<- engagement_data %>% 
  filter(quoted_user_id%in%eu_data) %>% 
  group_by(user_id) %>% 
  tally(name = "quote_count") %>% 
  right_join(.,all_users,by ="user_id") %>% 
  mutate(quote_count = replace_na(quote_count,0))

reply_count<- engagement_data %>% 
  filter(reply_to_user_id%in%eu_data) %>% 
  group_by(user_id) %>% 
  tally(name = "reply_count") %>% 
  right_join(.,all_users, by = "user_id") %>% 
  mutate(reply_count = replace_na(reply_count, 0))

engagement_counts<- left_join(retweet_data,quote_count, by = "user_id") %>% 
  left_join(.,reply_count,by = "user_id")

engagement_counts<- engagement_counts %>% 
  mutate(intermediary_retweet = case_when(retweet_count>0~1,.default = 0),
         intermediary_quote = case_when(quote_count>0~1,.default = 0),
         intermediary_reply = case_when(reply_count>0~1,.default = 0))

engagement_counts$intermediary_audience<- ifelse(engagement_counts$intermediary_retweet == 1 | engagement_counts$intermediary_quote ==1 | engagement_counts$intermediary_reply == 1,1,0)

intermediary_audience<- engagement_counts %>% 
  group_by(intermediary_audience) %>% 
  tally() %>% 
  mutate(perc = round(n/nrow(engagement_counts)*100)) %>% 
  mutate(intermediary_audience = recode(intermediary_audience, `1` = "Intermediary audience",`0` = "final audience"))
  

engagement_long<- engagement_counts %>% 
  filter(intermediary_audience == 1) %>% 
  mutate(all_three = ifelse(intermediary_retweet==1&intermediary_quote==1&intermediary_reply == 1,1,0)) %>% 
  select(-intermediary_audience) %>% 
  pivot_longer(cols = intermediary_retweet:all_three,names_to = "intermediary_engagement",values_to = "dummy") %>% 
  group_by(intermediary_engagement,dummy) %>% 
  tally() %>% 
  mutate(perc = round((n/99365)*100))

intermediary_audience_graph<- intermediary_audience %>% 
  ggplot(aes(x=intermediary_audience,y = perc ))+
  geom_bar(aes(fill = perc), stat = "identity",position = "dodge",show.legend = F)+
  theme_bw()+
  labs(x = "",y = "Percentage of audience",subtitle = "N of users = 210504")+
  scale_y_continuous(breaks = seq(0,100,5))


intermediary_audience_mode_type<- engagement_long %>% 
  mutate(dummy = recode(dummy, `1` = "yes",`0`="no"),
         intermediary_engagement = recode(intermediary_engagement,"all_three" = "all three",
                                          "intermediary_quote" = "quoting",
                                          "intermediary_reply" = "replying",
                                          "intermediary_retweet" = "retweeting")) %>%
  ggplot(aes(x = intermediary_engagement,y = perc))+
  geom_bar(aes(fill=dummy),stat= "identity",position = "stack")+
  theme_bw()+
  labs(x = "Engagement type",y = "Percentage of audience", subtitle = "N of users = 99365")+
  theme(legend.title=element_blank())+
  scale_y_continuous(breaks = seq(0,100,5))

remove(all_users,engagement_data,quote_count,retweet_data,reply_count,engagement_long)

###individual distributions.

diffusion_data<- read_feather(path = here("Data","Diffusion_data","l1_diffusion_data.feather")) %>% 
  filter(engagement_rate > 0) 

engagement_rate<- diffusion_data %>% 
  group_by(user_id,engagement_type) %>% 
  summarise(total_engagement = sum(engagement_rate))
network_size <- diffusion_data %>% 
  select(user_id,followers_count) %>% 
  distinct(user_id,.keep_all = T)

diffusion_capacity<- left_join(engagement_rate,network_size,by = "user_id") %>% 
  mutate(diffusion_capacity = (total_engagement*followers_count)) %>% 
  ggplot(aes(x = engagement_type,y = diffusion_capacity))+
  geom_jitter(aes(alpha = diffusion_capacity, color = engagement_type),width = .3,show.legend = F)+
  theme_bw()+
  labs(x = "engagement type", y = "diffusion capacity")+
  scale_x_discrete(labels = c("reply_count" = "by replying", "quote_count" = "by quoting", "retweet_count" = "by retweeting"))
  
eu_info<- readxl::read_excel(path = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim/PHD/Papers/general_eu_accounts_information.xlsx",sheet = 1) %>% 
  select(user_id,Actor_type) %>% 
  mutate(user_id = gsub("x_","",user_id)) %>% 
  mutate(Actor_type = recode(Actor_type, "High representative and vice president" = "Executive vice-president"))

figure_7_data<- left_join(diffusion_data, eu_info, by = c("engaged_user" = "user_id")) %>% 
  group_by(Actor_type,user_id) %>% 
  summarise(average_diffusion_cap = mean(diffusion_rate))

figure_7_graph<- figure_7_data %>% 
  drop_na() %>% 
  ggplot(aes(x = Actor_type, y = average_diffusion_cap))+
  geom_jitter(aes(alpha = average_diffusion_cap, color =Actor_type),width = .2,show.legend = F)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = .5))+
  labs(x = "Executive type", y = "Diffusion capacity of the followers")
