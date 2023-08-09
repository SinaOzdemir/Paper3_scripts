# İdentifying the optimal number of clusters (scaled data)


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","tidyLPA"))


# path --------------------------------------------------------------------


data_folder<- "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/audience_clustering_data"

results_folder<- "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/clustering_results"
# data --------------------------------------------------------------------

data<- readRDS(file = paste0(data_folder,"/clustering_data_raw.RDS")) %>% column_to_rownames(var = "user_id")


# function ----------------------------------------------------------------

lpa_estimator<-function(data,cluster_count,model){
  lpa_model<- tidyLPA::estimate_profiles(df = data,n_profiles = cluster_count,models = model)
  lpa_estimates<- tidyLPA::get_estimates(lpa_model)
  lpa_fits<- tidyLPA::get_fit(lpa_model)
  lpa_data<- tidyLPA::get_data(lpa_model)
  return(list(estimates = lpa_estimates,fit_indices = lpa_fits,dataset = lpa_data))
}


# building models ---------------------------------------------------------
lpa_results<-list()
for (i in 1:1000) {
  cat("bootstrap itteration: ",i,"\n")
  sample_data <- sample_frac(tbl = data,size = .01,replace = T)
  lpa_results[[i]]<- lpa_estimator(data = sample_data,
                                   cluster_count = 1:113,
                                   model = 1) #equal variance and covariances both fixed to 0
  saveRDS(lpa_results,file = paste0(results_folde,"/raw_data_lpa_model2.RDS"))
  
}
