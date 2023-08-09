##############################################################
# Title: EU executives' audience clustering                  #
# Author: Sina Özdemir                                       #
#         PhD candidate                                      #
#         Department of Sociology and Political Science      #
#         Norwegian University of Science and Technology     #
#         sina.ozdemir@ntnu.no                               #
##############################################################



# setup -------------------------------------------------------------------

require(pacman)

packs<- c("tidyverse","igraph","jaccard","NbClust","here","feather","vegan")

p_load(char = packs, install = T)


# data --------------------------------------------------------------------

flw_dir<- here("Data","L1_network_data")

flw_df<- list.files(path = flw_dir,pattern = "*_eu.feather",full.names = T) %>% 
  map_dfr(.,.f= read_feather) %>% distinct(eu_accounts,user_id) %>% ungroup() %>% 
  mutate(follows = 1)
#why in the fuck rows are matrix?

flw_df_w<- pivot_wider(data = flw_df,names_from = eu_accounts,values_from = follows, values_fill = 0) %>% 
  column_to_rownames("user_id")

remove(flw_df)


# jaccard similarity of users ---------------------------------------------


#(aka asymmetric binary): The vectors are regarded as binary bits, so non-zero elements are ‘on’ and zero elements are ‘off’.
#The distance is the proportion of bits in which only one is on amongst those in which at least one is on.
#bin_user_sim<- stats::dist(x = flw_portion,method = "binary")

#this properly converts everything to an integer matrix
flw_matrix<- flw_df_w %>% 
  mutate(across(everything(),~as.integer(.x))) %>% 
  data.matrix()

#Jaccard index is computed as 2B/(1+B), where B is Bray–Curtis dissimilarity.
#vegdist also has a wide range of similarity measures
a<- stats::dist(x = flw_matrix, method = "binary")

b<- vegan::vegdist(x = flw_matrix, method = "jaccard")


#they return the same values, only vegdist has a wider range of use

#Error: cannot allocate vector of size 168.8 Gb

#NTNU cluster gave the same error, there is probably a limit on how much ram it can use in one instance either by R or by admin

#Don't know how to use LMU cluster yet


############batching the data


a<- vegdist(x = flw_matrix[1:100,],method = "jaccard")

b<- vegdist(x = flw_matrix[101:200,],method = "jaccard")

c<- vegdist(x = flw_matrix[1:200,], method = "jaccard")



# alternative methods -----------------------------------------------------


#dist matrix is a weird object. Full similarity matrix would have 45.306.399.609 elements (43 billion). This going to be a little bit of a challenge. 

## Possible solutions
#1) Reduce the matrix only to distinc following behavior then cluster
#2) Monte Carlo bootstrapping clustering


#Option 1)

reduced_flw_matrix<- flw_df_w %>% distinct(across(everything()))

user_jac_dist<- stats::dist(x = reduced_flw_matrix,method = "binary")
#Error: cannot allocate vector of size 21.0 Gb

#So this helped but didn't solve the problem
#probably because the distance matrix from reduced 75027 users would
#make 5.629.050.729 element matrix


#option 2: Monte Carlo approach

# step by step
start_time<- Sys.time()
#first try with 5% of the sampe (10642x117) takes a long time
#reducing the sample size to 2% (4257x117)
#reducing the sample size to 1000 and using the reduced data

test_reduced<- flw_df_w %>% distinct(across(everything()))

test_sample<- slice_sample(.data = test_reduced,n = 1000,replace = T)

test_stat<-c("kl",
             "ch",
             "hartigan",
             "ccc", #Error: division by zero!???
             "scott",
             "marriot",
             "trcovw",
             "tracew",
             "friedman",
             "rubin",
             "cindex",
             "db",
             "silhouette",
             "beale",
             "ratkowsky",
             "ball",
             "ptbiserial",
             "gap",
             "mcclain",
             "dunn",
             "sdindex",
             "sdbw")

##hubert and d index are graphical tests, should be run seperately
## gamma,gplus,tau takes too long to run
#this can probably be parellelized 
test_results<-data.frame()

try(expr = {for (i in 1:length(test_stat)) {
  
  cat("calculating ",test_stat[i],"\n")
  
  obj_clust_test_err<- NbClust(data = test_sample,
                               distance = "binary",
                               min.nc = 1,
                               max.nc = 20,#max nc is a problem
                               method = "ward.D2",
                               index = test_stat[i])
  
  
  obj_clust_i_res<- data.frame(index_name = test_stat[i],
                               n_clust = obj_clust_test_err$Best.nc["Number_clusters"],
                               index_val = obj_clust_test_err$Best.nc["Value_Index"])
  test_results<- rbind(test_results, obj_clust_i_res)
  
}
},silent = T)

finish_time<- Sys.time()

benchmark<- finish_time-start_time

##Still takes a ridiculous amount of time