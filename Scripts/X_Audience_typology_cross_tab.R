
# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","feather","here","xlsx")

p_load(char = packs)

eu_info<- read.xlsx(file = here("Data","general_eu_accounts_information.xlsx"),sheetIndex = 1) %>% 
  mutate(user_id = gsub("x_","",.$user_id))



# data --------------------------------------------------------------------


audience_policy_interest<- readRDS(file = here("Data","results_data","audience_interest_diversity.RDS")) %>% 
  distinct(user_id,.keep_all = T)

audience_policy_interest_sc<- readRDS(file = here("Data","results_data","audience_interest_diversity_special_case.RDS")) %>% 
  distinct(user_id,.keep_all = T) %>% 
  select(-eu_accounts)

colnames(audience_policy_interest_sc)<- colnames(audience_policy_interest)

audience_policy_interest_no_sc<- audience_policy_interest %>% 
  filter(!(user_id%in%audience_policy_interest_sc$user_id))


final_audience_policy_interest<- rbind(audience_policy_interest_sc,audience_policy_interest_no_sc) %>% select(user_id,interest_breadth)


audience_diffusion<- read_feather(path = here("Data","Diffusion_data","l1_diffusion_data.feather"))

audience_diffusion<- audience_diffusion %>% select(user_id,diffusion_rate) %>% distinct(user_id,.keep_all = T)

audience_typology_data<- inner_join(final_audience_policy_interest,audience_diffusion, by = "user_id")

#clean the environment to minimize the ram usage
# ps. something fishy going on about number of audience

rm(list = grep(pattern = "audience_typology_data",x = ls(),value = T,invert = T))



# audience_classification 

# mode classification ----
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_interest_breadth<- getmode(v = audience_typology_data$interest_breadth)

mode_diffusion_typology<- getmode(v = audience_typology_data$diffusion_rate)

mode_audience_typology<- audience_typology_data %>% 
  mutate(interest_diversity_cat = ifelse(interest_breadth>=mode_interest_breadth,"Diverse","Narrow"),
         diffusion_rate_cat = ifelse(diffusion_rate>=mode_diffusion_typology,"High","Low")) %>% 
  mutate(audience_type = ifelse(interest_diversity_cat == "Diverse"&diffusion_rate_cat == "High","Super-spreader",
                                ifelse(interest_diversity_cat == "Diverse" & diffusion_rate_cat == "Low","Lurker",
                                       ifelse(interest_diversity_cat == "Narrow" & diffusion_rate_cat == "High","Policy-apostle",
                                              ifelse(interest_diversity_cat == "Narrow" & diffusion_rate_cat == "Low","Policy-anorak",NA))))) %>% 
  group_by(audience_type) %>% 
  tally(name = "frequency") %>% 
  mutate(perc_share = round(frequency/nrow(audience_typology_data),2))



mode_audience_typology %>% 
  ggplot(aes(x = audience_type,y = perc_share))+
  geom_bar(aes(fill = perc_share), stat = "identity",position = "dodge")

### When mode is used to divide audience into sub-categories there appears only 2 type: policy apostle and super-spreader
### this is due to 2 factors. First a good amount of audience follows the political leadership of the EU so it skews the counting of
### high interest diversity. Similarly, most audience don't engage with Eu messages, if mode is taken to indicate audience engagement
### it skews the distribution towards high (assuming it is taken as >=)




# median classification----


median_interest_breadth<- median(x =  audience_typology_data$interest_breadth)

median_diffusion_typology<- median(x = audience_typology_data$diffusion_rate)

median_audience_typology<- audience_typology_data %>% 
  mutate(interest_diversity_cat = ifelse(interest_breadth>=median_interest_breadth,"Diverse","Narrow"),
         diffusion_rate_cat = ifelse(diffusion_rate>=median_diffusion_typology,"High","Low")) %>% 
  mutate(audience_type = ifelse(interest_diversity_cat == "Diverse"&diffusion_rate_cat == "High","Super-spreader",
                                ifelse(interest_diversity_cat == "Diverse" & diffusion_rate_cat == "Low","Lurker",
                                       ifelse(interest_diversity_cat == "Narrow" & diffusion_rate_cat == "High","Policy-apostle",
                                              ifelse(interest_diversity_cat == "Narrow" & diffusion_rate_cat == "Low","Policy-anorak",NA))))) %>% 
  group_by(audience_type) %>% 
  tally(name = "frequency") %>% 
  mutate(perc_share = round(frequency/nrow(audience_typology_data),2))



median_audience_typology %>% 
  ggplot(aes(x = audience_type,y = perc_share))+
  geom_bar(aes(fill = perc_share), stat = "identity",position = "dodge")

# median policy interest and diffusion behavior provides a better picture of the situation


# mean classification----


mean_interest_breadth<- round(mean(x =  audience_typology_data$interest_breadth))

mean_diffusion_typology<- round(mean(x = audience_typology_data$diffusion_rate))

mean_audience_typology<- audience_typology_data %>% 
  mutate(interest_diversity_cat = ifelse(interest_breadth>=mean_interest_breadth,"Diverse","Narrow"),
         diffusion_rate_cat = ifelse(diffusion_rate>=mean_diffusion_typology,"High","Low")) %>% 
  mutate(audience_type = ifelse(interest_diversity_cat == "Diverse"&diffusion_rate_cat == "High","Super-spreader",
                                ifelse(interest_diversity_cat == "Diverse" & diffusion_rate_cat == "Low","Lurker",
                                       ifelse(interest_diversity_cat == "Narrow" & diffusion_rate_cat == "High","Policy-apostle",
                                              ifelse(interest_diversity_cat == "Narrow" & diffusion_rate_cat == "Low","Policy-anorak",NA))))) %>% 
  group_by(audience_type) %>% 
  tally(name = "frequency") %>% 
  mutate(perc_share = round(frequency/nrow(audience_typology_data),2))



mean_audience_typology %>% 
  ggplot(aes(x = audience_type,y = perc_share))+
  geom_bar(aes(fill = perc_share), stat = "identity",position = "dodge")

## probably best to use median for policy interest breadth but mean for diffusion

# final results----

median_interest_breadth<- median(x =  audience_typology_data$interest_breadth)
mode_interest_breadth<- getmode(v = audience_typology_data$interest_breadth)

audience_typology_data %>% 
  ggplot(aes(x= as.factor(interest_breadth)))+
  geom_bar(stat = "count")

quantile(x = audience_typology_data$interest_breadth)

#this return 10 
mean_diffusion_typology<- round(mean(x = audience_typology_data$diffusion_rate))

final_audience_typology<- audience_typology_data %>% 
  mutate(interest_diversity_cat = ifelse(interest_breadth>=median_interest_breadth,"Diverse","Narrow"),
         diffusion_rate_cat = ifelse(diffusion_rate>=mean_diffusion_typology,"High","Low")) %>% 
  mutate(audience_type = ifelse(interest_diversity_cat == "Diverse"&diffusion_rate_cat == "High","Super-spreader",
                                ifelse(interest_diversity_cat == "Diverse" & diffusion_rate_cat == "Low","Lurker",
                                       ifelse(interest_diversity_cat == "Narrow" & diffusion_rate_cat == "High","Policy-apostle",
                                              ifelse(interest_diversity_cat == "Narrow" & diffusion_rate_cat == "Low","Policy-anorak",NA)))))
  
final_audience_typology_summary<- final_audience_typology %>% 
  group_by(audience_type) %>% 
  tally(name = "frequency") %>% 
  mutate(perc_share = round(frequency/nrow(audience_typology_data),2))


final_audience_typology %>% 
  ggplot(aes(x = audience_type,y = perc_share))+
  geom_bar(aes(fill = perc_share), stat = "identity",position = "dodge")+
  theme_bw()+
  labs(y = "Percentage share of the audience", x= "Audience type",title = "Composition of EU executives' audience on Twitter by type")+
  guides(fill = "none")


# percentage share of the audience type by executive type -----------------


audience_typology<- final_audience_typology %>% select(user_id,audience_type)


executive_follower_list_a<- read_feather(path = here("Data","L1_network_data","L1_flw_network_eu.feather"))

exeutive_follower_list_b<- read_feather(path = here("Data","L1_network_data","L1_eng_network_eu.feather"))

executive_follower_list<- rbind(executive_follower_list_a,exeutive_follower_list_b)


audience_type_by_executive<- eu_info %>% select(user_id,Actor_type) %>% rename(eu_accounts = user_id) %>% 
  left_join(executive_follower_list,.,by="eu_accounts") %>% 
  drop_na(Actor_type)

audience_type_by_executives<- audience_type_by_executive %>% 
  left_join(x = .,y = audience_typology,by = "user_id")


audience_type_by_executives<- audience_type_by_executives %>% drop_na(audience_type)


audience_type_by_executives_summary<- audience_type_by_executives %>% 
  group_by(Actor_type,audience_type) %>% 
  tally()

audience_size <- audience_type_by_executives %>% 
  group_by(Actor_type) %>% 
  tally(name = "audience_size")

audience_type_by_executives_summary<- left_join(audience_type_by_executives_summary,audience_size, by = "Actor_type")


audience_type_by_executives_summary<- audience_type_by_executives_summary %>% 
  mutate(perc_share_audience_type = round((n/audience_size),2))

audience_type_by_executives_summary %>% 
  ggplot(aes(x = audience_type, y = perc_share_audience_type))+
  geom_bar(aes(fill = perc_share_audience_type),stat = "identity", position = "dodge",show.legend = F)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1, vjust = 1))+
  labs(x = "Audience type",y = "Percentage share")+
  facet_wrap(~Actor_type)