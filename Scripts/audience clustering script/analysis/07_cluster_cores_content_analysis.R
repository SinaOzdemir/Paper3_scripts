# engagement extractor:


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","quanteda","quanteda.textstats","rvest","ggraph","tidygraph","patchwork","quanteda.textplots"))



# paths -------------------------------------------------------------------

followers_path = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/follower_tweets"

replier_path = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/replier_tweets"

results_path = "M:/cluster count/results"


# user content extraction ----------------------------------------------------------
user_files = c(list.files(followers_path,
                          pattern ="*.RDS",
                          full.names = T ),
               list.files(replier_path,
                          pattern = "*.RDS",
                          full.names = T))
cluster_cores <- readRDS(file = paste0(results_path,"/kmeans_core_users_desc_df.RDS")) %>% mutate(user_id = gsub("x_","",user_id)) %>% pull(user_id)




cluster_core_user_contet<- data.frame()
for (i in 1:length(user_files)) {
  user_engagement = data.frame()
  cat("examining user batch ",i,"\n")
  users = readRDS(user_files[i])
  
  for (j in 1:length(users)) {
    cat("processing user ",j,"\n")
    user = users[[j]] 
    
    if(nrow(user)==0){
      next
      
    }else{
      core_user<- user %>% filter(user_id%in%cluster_cores)
      if(nrow(core_user)==0){
        next
        
      }else{
        cluster_core_user_contet<- rbind(cluster_core_user_contet,core_user)
      }
    }
    
    
    
    } 
  
  saveRDS(cluster_core_user_contet,file = paste0(results_path,"/kmeans_core_users_content.RDS"))
}





# user profile description analysis ---------------------------------------

p_load(char = c("rvest","ggraph","tidygraph","patchwork"))

load(file = paste0(results_path,"/kmeans_n3_clustering_results.Rdata"))



user_data<-readRDS(file = paste0(results_path,"/kmeans_core_users_content.RDS")) %>% 
  mutate(user_id = paste0("x_",user_id)) %>% 
  left_join(.,clusters,by = "user_id")
  

user_profile_description<-user_data %>% select(user_id,cluster,lang,place_url,place_name,
                                               place_full_name,place_type,country,
                                               country_code,location,description,
                                               followers_count,friends_count,
                                               statuses_count,favorite_count) %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  mutate(cluster = recode(cluster, `1` = "Super-spreaders",`2` = "Casual followers", `3` = "Lurkers"))
  
total_cluster_sizes<- clusters %>%
  mutate(cluster = as.factor(cluster)) %>% 
  mutate(cluster = recode(cluster, `1` = "Super-spreaders",`2` = "Casual followers",`3` = "Lurkers")) %>% 
  group_by(cluster) %>%
  tally() %>% 
  mutate(perc = round(n/nrow(clusters)*100)) %>% 
  ggplot(aes(x = cluster,y = perc))+
  geom_bar(aes(fill = perc), stat = "identity", position = "dodge",show.legend = F)+
  theme_bw()+
  labs(x = "cluster ID", y = "Cluster size as percentage of total users")



tweet_counts_by_cluster <-
  user_profile_description %>% 
  group_by(cluster) %>% 
  tally(name = "tweet_count")

#language

languages<- user_profile_description %>% 
  group_by(cluster,lang) %>% 
  tally() %>% 
  left_join(.,tweet_counts_by_cluster,by = "cluster") %>% 
  mutate(perc = round((n/tweet_count)*100))

language_codes<- data.frame(language_code = read_html(x = "https://en.wikipedia.org/wiki/List_of_ISO_639-2_codes") %>% 
  html_elements(css = "#iso-codes td:nth-child(4)") %>% html_text2(),
  language_name = read_html(x = "https://en.wikipedia.org/wiki/List_of_ISO_639-2_codes") %>% 
    html_elements(css = "#iso-codes td:nth-child(5)") %>% html_text2()) %>% 
  filter(language_code != "")


languages_graph<- languages %>% 
  filter(perc>0) %>% 
  left_join(.,language_codes, by = c("lang" = "language_code")) %>% 
  mutate(language_name = replace_na(language_name,"emoji,hashtag or mention")) %>% 
  ggplot(aes(x = language_name,y = perc))+
  geom_bar(aes(fill = perc),stat = "identity",position = "dodge",show.legend = F)+
  coord_flip()+
  theme_bw()+
  labs(x = "Language",y = "Percentage share of a language in a cluster")+
  facet_wrap(~cluster)

#location

location <- user_profile_description %>% 
  distinct(user_id,.keep_all = T) %>% 
  group_by(cluster,location) %>% 
  tally() %>% 
  filter(location != "") %>% 
  mutate(location = recode(location,
                           "Alicante" = "Spain",
                           "Bremen, Planet Earth" = "Germany",
                           "Highgate, London " ="The UK",
                           "Lowestoft, England or, Hell" = "The UK",
                           "Nairobi, Kenya" = "Kenya",
                           "Perry, Birmingham, Gaia" = "The UK",
                           "Walsall, England" = "The UK",
                           "Ankara, Türkiye" = "Turkey",
                           "Bad Rothenfelde, Deutschland" = "Germany",
                           "Oxford, England" = "The UK",
                           "Pescara, Abruzzo" = "Italy",
                           "italia" = "Italy",
                           "Paris, France" = "France",
                           "Lubumbashi" = "Congo",
                           "Jabalpur" = "India",
                           "NYC* DC* India*FL*CA*China" = "US",
                           "Rotterdam" = "The Netherlands",
                           )) %>% 
  group_by(cluster,location) %>% 
  tally() %>% 
  mutate(perc = (n/10)*100)

#there is probably a better way of visualising this
location_graph<- location %>% 
  ggplot(aes(x = location, y = perc))+
  geom_bar(aes(fill =perc),stat = "identity",position = "dodge",show.legend = F)+
  theme_bw()+
  coord_flip()+
  labs(y = "percentage of cluster centroids",x = "Country",subtitle = "Country of cluster centroids")+
  facet_wrap(~cluster)
                           
#network size

network_information<- user_profile_description %>% 
  distinct(user_id,.keep_all = T) %>% 
  group_by(cluster) %>% 
  filter(case_when(cluster == "Super-spreaders"~ statuses_count<191414,
                   cluster == "Casual followers"~ statuses_count<6263,
                   cluster == "Lurkers" ~ statuses_count<39)) %>% 
  summarise(avg_tweet_count =mean(statuses_count),
            avg_follower_count = mean(followers_count),
            avg_friends_count = mean(friends_count)) %>% 
  ungroup()

network_information_graph<- network_information %>% 
  pivot_longer(cols = avg_tweet_count:avg_friends_count,names_to = "network_info",values_to = "avg") %>% 
  mutate(network_info = recode(network_info,
                               "avg_tweet_count" = "number of tweets",
                               "avg_follower_count" = "number of followers",
                               "avg_friends_count" = "number of friends")) %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  ggplot(aes(x = network_info, y = avg, group = cluster))+
  geom_point(aes(shape = cluster),size = 6, color = "steelblue")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = .5))+
  facet_wrap(~cluster)+
  labs(y = "Mean values", x = "", caption = "cluster specific extreme outliers are removed",subtitle = "Network information of cluster centroids")
  
library(patchwork)


location_graph+network_information_graph

# tweet_count <-user_profile_description %>% 
#   distinct(user_id,.keep_all = T) %>% 
#   filter(case_when(cluster==2 ~ statuses_count<6000,
#                    T~statuses_count <15000)) %>% 
#   ggplot(aes(x = as.factor(cluster), y = statuses_count))+
#   geom_boxplot()+
#   theme_bw()+
#   labs(x = "Number of tweets", y = "Cluster ID", subtitle = "Extreme outliers are removed")
#   
# 
# follower_distribution<- user_profile_description %>% 
#   distinct(user_id,.keep_all = T) %>% 
#   filter(case_when(cluster == 1 ~ followers_count<7521,
#                    cluster == 2 ~ followers_count<528,
#                    cluster == 3 ~ followers_count<120)) %>% 
#   ggplot(aes(x = as.factor(cluster), y = followers_count))+
#   geom_boxplot()+
#   theme_bw()+
#   labs(x = "Number of followers", y = "Cluster ID",caption = "cluster specific outliers are removed")
#   
# 
# friends_distribution<- user_profile_description %>% 
#   distinct(user_id,.keep_all = T) %>% 
#   ggplot(aes(x = as.factor(cluster), y = friends_count))+
#   geom_boxplot()+
#   theme_bw()+
#   labs(x = "Number of friends", y = "Cluster ID")
  

network_size_graph<- tweet_count + follower_distribution + friends_distribution


# user content analysis --------------------------------------------------------

#devtools::install_github(repo = "https://github.com/kbenoit/sophistication")
#home.ansatt.ntnu is causing problem in installing sophistication.
#I should do this analysis on a linux or home pc



load(file = paste0(results_path,"/kmeans_n3_clustering_results.Rdata"))


user_data<-readRDS(file = paste0(results_path,"/kmeans_core_users_content.RDS")) %>% 
  mutate(user_id = paste0("x_",user_id)) %>% 
  left_join(.,clusters,by = "user_id")

user_content_data<- user_data %>%
  select(user_id,status_id,cluster,text) %>%
  distinct(status_id,.keep_all =T) %>% 
  group_by(cluster) %>% 
  summarise(text = paste(text,collapse = ". ")) %>% 
  mutate(text = str_remove_all(pattern = "&amp",text)) %>% 
  mutate(text = str_remove_all(pattern = "#\\w+|\\@w+",text)) %>% 
  mutate(text = qdapRegex::rm_url(text,clean = T,trim = T)) %>% 
  mutate(text = qdapRegex::rm_twitter_url(text,trim = T,clean = T)) %>% 
  mutate(text = str_remove_all(pattern = emo::ji_rx,text))

langs<- user_data %>% distinct(lang) %>% pull(lang)

all_stopwords<- c()
available_languages<- stopwords::stopwords_getlanguages(source = "stopwords-iso")
for (i in 1:length(langs)) {
  if(langs[i]%in%available_languages){
  stopword<- stopwords::stopwords(language = langs[i],source = "stopwords-iso")
all_stopwords<- c(all_stopwords,stopword)
}else{
  next
}}

user_corpus<- corpus(user_content_data, docid_field = "cluster",text_field = "text")

tokens<- user_corpus %>% 
  tokens(remove_punct = T,remove_symbols = T,remove_numbers = T,remove_url = T,include_docvars = T) %>% 
  tokens_select(pattern = all_stopwords,selection = "remove") %>% 
  tokens_select(min_nchar = 2,selection = "remove") %>% 
  tokens_wordstem()

dfm<- tokens %>% 
  dfm()

saveRDS(dfm,file = "M:/cluster count/user_content_dfm_raw.RDS")

dfm_trimed<- dfm %>% 
  dfm_trim(min_termfreq = .20,max_termfreq = .8,termfreq_type = "quantile")

saveRDS(dfm,file = "M:/cluster count/user_content_dfm_trimmed.RDS")

#wordclouds

cluster_dfm<- readRDS(file = "M:/cluster count/user_content_dfm_raw.RDS")

cluster1_dfm<- cluster_dfm %>% 
  dfm_subset(docid(.)==1) %>% 
  dfm_remove("@*") %>% 
  textplot_wordcloud(min_count = 1,max_words = 100,color = "darkblue")

cluster2_dfm<- cluster_dfm %>% 
  dfm_subset(docid(.)==2) %>% 
  dfm_remove("@*") %>% 
  textplot_wordcloud(min_count = 1,max_words = 100,color = "darkgreen")

cluster3_dfm<- cluster_dfm %>% 
  dfm_subset(docid(.)==3) %>% 
  dfm_remove("@*") %>% 
  textplot_wordcloud(min_count = 1,max_words = 100,color = "darkred")

#lexical diversity

lex_div<- textstat_lexdiv(cluster_dfm,measur = "TTR") %>% 
  ggplot(aes(x = document, y = TTR))+
  geom_bar(aes(fill = TTR),stat = "identity",position = "dodge")+
  theme_bw()+
  labs(x = "cluster ID",y = "Type-Token Ratio",title = "Lexical diversity of clusters")

# keyness

## compares frequencies of words between target and refrence documents

cluster1_2_dfm<- cluster_dfm %>% 
  dfm_remove("@*") %>% 
  dfm_subset(docid(.)!=3)

cluster1_2_key<- textstat_keyness(x = cluster1_2_dfm,target = docid(cluster1_2_dfm)==1) %>% 
  textplot_keyness()


cluster3_2_dfm<- cluster_dfm %>% 
  dfm_remove("@*") %>% 
  dfm_subset(docid(.)!=1)

cluster3_2_key<- textstat_keyness(x = cluster3_2_dfm,target = docid(cluster3_2_dfm)==2) %>% 
  textplot_keyness()


cluster1_3_dfm<- cluster_dfm %>% 
  dfm_remove("@*") %>% 
  dfm_subset(docid(.)!=2)

cluster1_3_key<- textstat_keyness(x = cluster1_3_dfm,target = docid(cluster1_3_dfm)==1) %>% 
  textplot_keyness()

# profile descriptions: (10 from each cluster is not informative enough to make a claim, these are all highly educated peopl)

profileDescData<- user_data %>%
  distinct(user_id,.keep_all = T) %>%
  select(user_id,description,cluster) %>%
  filter(description !="")

library(xlsx)

openxlsx::write.xlsx(x = profileDescData,file = here("Data","clustering_results","centroid_profile_descriptions.xlsx"))
