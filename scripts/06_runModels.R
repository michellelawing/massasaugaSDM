##################
###--- Maxent ----
##################

# Run and save models  

#is maxent here?
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
file.exists(jar)

#Combinations of occurrences, backgrounds, and climates as model inputs
# ((4 filters X 5 folds = 20 presences combos) X 
# (4 buffer types X 2 sample sizes = 8 background combos) = 160 input sets) X 
# 3 climates = 480 models
model_tracker <- array(NA, dim = c(480, 4))
climate_list  <- list(bioclim_1, bioclim_2, bioclim_3)
pick_climate  <- list("predictors1","predictors2","predictors3")
pick_filter   <- list("None", "Env.15", "Env.3", "Env.75")
pick_fold     <- list("Random","West","East","North","South")
pick_back     <- levels(background$Set)
model_list    <- list()
# 
start_time <- Sys.time()

counter <- 1
for (climate in 1:3){
  for (filters in 1:4){
    for (folds in 1:5) {
      for (backs in 1:8){
        model_tracker[counter,] <- c(pick_climate[[climate]], pick_filter[[filters]],
                                     pick_fold[[folds]], pick_back[[backs]])
        input_temp <- input_data[input_data$Filter == pick_filter[[filters]] &
                                   input_data$Fold == pick_fold[[folds]] &
                                   input_data$Training == "Yes",1:2]
        back_temp <- background[background$Set == pick_back[[backs]] &
                                  background$Training == "Yes", 1:2]
        path_temp <- paste("results/spring2020/modelruns/climate", climate, ".filter", pick_filter[[filters]],".fold", pick_fold[[folds]], ".back", pick_back[[backs]], sep = "")
        model_list[[counter]] <- maxent(climate_list[[climate]], p = input_temp, a = back_temp, path = path_temp, args = c('threads=4'))
        #model_list[[counter]] <- maxent(climate_list[[climate]], p = input_temp, a = back_temp, path = path_temp, args = c('betamultiplier=1', 'hinge=false','threads=4'))
        counter <- counter + 1
      }
    }
  }
}

end_time <- Sys.time()
end_time - start_time

#save out the model list and model tracker
save(model_list, file = "results/spring2020/modelruns/model_list.RData")
save(model_tracker, file = "results/spring2020/modelruns/model_tracker.RData")

#load model list and model tracker output from a previous run
# load("results/spring2020/modelruns_20200212_largecircles/model_list.RData")
# load("results/spring2020/modelruns_20200212_largecircles/model_tracker.RData")
