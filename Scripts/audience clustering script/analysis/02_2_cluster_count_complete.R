#cluster number determination


# packages ----------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","NbClust"))


# directories -------------------------------------------------------------

data_folder<- "M:/cluster count/audience_clustering_data"

# dataset -----------------------------------------------------------------

clustering_data = readRDS(file = paste0(data_folder,"/clustering_df_scaled.RDS")) %>% as.data.frame.matrix()



# test --------------------------------------------------------------------

test_data<- clustering_data[1:100,]

clustering_indices<- c("kl", "ch", "hartigan", "ccc", "scott", "marriot",
                       "trcovw", "tracew", "friedman", "rubin", "cindex",
                       "db", "silhouette", "duda", "pseudot2",
                       "ratkowsky", "ball", "ptbiserial", "gap", "frey",
                       "mcclain", "gamma", "gplus", "tau", "dunn",
                       "hubert", "sdindex", "dindex", "sdbw")

bad_indices<- c()
for (i in clustering_indices) {
  possible_error = tryCatch(NbClust(data = test_data,
                            distance = "euclidean",
                            min.nc = 2,
                            max = 5,
                            method = "centroid",
                            index = i),
           error = function(e)e)
  
  if(inherits(possible_error,"error")){
    bad_indices<- c(bad_indices,i)
    next
  }else{
  
  cluster_numbers = NbClust(data = test_data,
                            distance = "euclidean",
                            min.nc = 2,
                            max = 5,
                            method = "centroid",
                            index = i)
  }
}

good_indices<- clustering_indices[!clustering_indices %in% bad_indices]


test_results<- cluster_numbers[["Best.nc"]][1]
# implementation ----------------------------------------------------------

set.seed(12345)
optimum_numbers<- data.frame()
errored_index<-c()
dist = "euclidean"
methd = "complete"
for (i in 12:length(good_indices)) {
  index <- good_indices[i]
  cat("estimating optimum number of clusters with ", index,"\n")
  possible_error = tryCatch(NbClust(data = clustering_data[1:2105,],
                                    distance = dist,
                                    min.nc = 1,
                                    max.nc = 113,
                                    method = methd,
                                    index = index),
                            error = function(e)e)
  if(inherits(possible_error,"error")){
    errored_index<-c(errored_index,index)
    next
  }else{
    for (j in 1:100) {
      obs_index<- sample(x = 1:nrow(clustering_data),size = nrow(clustering_data)*.01,replace = T)
      
      bootstrap_data<- clustering_data[obs_index,]
      
      nb_analysis<- NbClust(data = bootstrap_data,
                            distance = dist,
                            min.nc = 1,
                            max.nc = 113,
                            method = methd,
                            index = index)
      nb_results<- data.frame(index = index,
                              itteration = j,
                              best_nc = nb_analysis[["Best.nc"]][1],
                              score = nb_analysis[["Best.nc"]][2])
      optimum_numbers<- rbind(optimum_numbers,nb_results)
    }
    saveRDS(optimum_numbers,file = paste0(data_folder,"/optimum_cluster_number_complete.RDS"))  
  }
  
}