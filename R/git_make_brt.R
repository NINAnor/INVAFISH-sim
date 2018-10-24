#########################
#      brt-model        #
#########################

#' Genneral information about BRT-models (Boosted Regression Trees)
#' Brt-models are a type of regression model technique which is very flexible. It thus has some benefits when fitting ecological data, which is usually non-linear and so on.
#' Given care in the model-fittingg, brt can give predictive advantages over methods as e.g. glm or gam.
#' The following script uses the dismo and gmb packages to optimise a brt model for the given species.
#' Analytically, BRT regularization involves jointly optimizing the number of  trees, learning rate and tree complexity.
#' Here optimizing numbers of trees is done through the gbm.step function, and the script includes a function that tries to aid in the optimazation prosess for learning rate and tree complexity (the get.train.diganostic.func).
#' The next step is to fit the actual model used for predictions (brt_mod)
#'  Its is recomended to read through "A working guide to boosted regression trees" (Elith, et al 2009), before atempting your first go.

#' Parameters to include
#' @param



###############################
# part 1: load and filter data
###############################
library(stringr)
library(gbm)
library(dismo)
library(dplyr)
library(doParallel)

focal_species_var<-"gjedde"
#source("./R/f_geoselect.R")
#outdata <- f_geoselect_inverse_spdf(geoselect="./Data/geoselect_native_Rutilus_rutilus.rds",inndata=outdata) #needs to be adressed
# make spatial selection for model estimation - Norway minus Finnmark, Troms and Nordland.
# The distribution and native area for finnamark would create a lot of missery

# e.g.
# outdata <- outdata_data_gjedde[outdata_data_gjedde$countryCode =="NO",]
# or
# outdata <- lake_env[lake_env$countryCode =="NO",]

outdata <- merge(inndata_timeslot[,c('waterBodyID', 'Esox_lucius')], lake_env, by='waterBodyID', all.y=FALSE)

outdata$countryCode <- factor(outdata$countryCode)
outdata <- outdata %>% filter(!(county %in% c("Finnmark","Troms","Nordland")))
outdata <- outdata %>% filter((county %in% c("Aust-Agder","Vest-Agder","Telemark","Rogaland")))
outdata$county <- factor(outdata$county)
# outdata <- outdata[outdata$minimumElevationInMeters>0,]
# outdata$n_pop <- NA
# outdata$n_pop <- ifelse(outdata$waterBodyID %in% geoselect_no_gjedde_pop_5000$waterBodyID,geoselect_no_gjedde_pop_5000$count,outdata$n_pop)



#focal_species_vec <- unique(outdata$focal_species)
# remove all populations of focal species where focal species is present at start of time-slot
# i.e. focal_specie. No idea how to do this in data.tables, but it's straith-forward with dplyr
# using the programmable version of functions identified by underscore at the end (in this case filter_)
#focal_species_var <- stringr::str_replace(string=focal_species_vec, pattern=" ", replacement="_")
#select_focal <- paste("!(",focal_species_var,"==1 & introduced==0)")
#analyse.df <- outdata %>% dplyr::filter_(select_focal)
analyse.df <- as.data.frame(outdata) # convert to data.frame - needed for gbm.step input


###############################
# part 2: Make the brt model
###############################


#It is encuraged to do this with paralell computing speeds the prosess up to some extent.
#Identify cores on current system
cores <- detectCores(all.tests = FALSE, logical = FALSE) - 2
# Outer loop has 9 items, the inner 5
cores

#Create training function for gbm.step
get.train.diganostic.func=function(tree.com,learn,indf){

  k.out=list(interaction.depth=NA,
                 shrinkage=NA,
                 n.trees=NA,
                 AUC=NA,
                 cv.AUC=NA,
                 deviance=NA,
                 cv.deviance=NA)

  #set seed for reproducibility
  k1<-try(gbm.step(data=indf,
               gbm.x = c( "distance_to_road_log", "dist_to_closest_pop_log","SCI","minimumElevationInMeters","buffer_5000m_population_2006" ,"area_km2_log","n_pop"), #  Include variables at will here,"county"
               gbm.y = "introduced",
               family = "bernoulli",
               tree.complexity = tree.com,
               learning.rate = learn,
               bag.fraction = 0.8,
               prev.stratify=TRUE,
               n.folds=10,
               n.trees=500,
               step.size=100,
               silent=TRUE,
               plot.main = FALSE,
               n.cores=1))

  if(exists("k1")) {
    if(! is.vector(k1)) {
      k.out=try(list(interaction.depth=k1$interaction.depth,
               shrinkage=k1$shrinkage,
               n.trees=k1$n.trees,
               AUC=k1$self.statistics$discrimination,
               cv.AUC=k1$cv.statistics$discrimination.mean,
               deviance=k1$self.statistics$mean.resid,
               cv.deviance=k1$cv.statistics$deviance.mean))
    }
    }
  return(k.out)
}

#define complexity and learning rate
tree.complexity<-c(1:9)
learning.rate<-c(0.01, 0.025, 0.005, 0.0025,0.001)

#setup parallel backend to use n processors
cl<-parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

start.time <- Sys.time()
#Run the actual function
gbms <- foreach(i = tree.complexity, .packages = c('gbm', 'dismo', 'doParallel')) %:%
  foreach(j = learning.rate, .packages = c('gbm', 'dismo', 'doParallel')) %dopar% {
    try(get.train.diganostic.func(tree.com=i,learn=j,indf=analyse.df))
}
end.time <- Sys.time()

#exec_time <- end.time - start.time
end.time - start.time

#Stop parallel
stopCluster(cl)
registerDoSEQ()

# Create data frame for collecting training results
train.results.par <- data.frame(tc=numeric(),
                            lr=numeric(),
                            interaction.depth=numeric(),
                            shrinkage=numeric(),
                            n.trees=numeric(),
                            AUC=numeric(),
                            cv.AUC=numeric(),
                            deviance=numeric(),
                            cv.deviance=numeric()
                            )

# Collect training results
for (i in 1:length(tree.complexity)) {
  for (j in 1:length(learning.rate)) {
    train.results.par[nrow(train.results.par) + 1,] <- list(
      tc=ifelse(is.null(tree.complexity[i]),NA,tree.complexity[i]),
      lr=ifelse(is.null(learning.rate[j]),NA,learning.rate[j]),
      interaction.depth=ifelse(is.null(gbms[[i]][[j]]$interaction.depth),NA,gbms[[i]][[j]]$interaction.depth),
      shrinkage=ifelse(is.null(gbms[[i]][[j]]$shrinkage),NA,gbms[[i]][[j]]$shrinkage),
      n.trees=ifelse(is.null(gbms[[i]][[j]]$n.trees),NA,gbms[[i]][[j]]$n.trees),
      AUC=ifelse(is.null(gbms[[i]][[j]]$AUC),NA,gbms[[i]][[j]]$AUC),
      cv.AUC=ifelse(is.null(gbms[[i]][[j]]$cv.AUC),NA,gbms[[i]][[j]]$cv.AUC),
      deviance=ifelse(is.null(gbms[[i]][[j]]$deviance),NA,gbms[[i]][[j]]$deviance),
      cv.deviance=ifelse(is.null(gbms[[i]][[j]]$cv.deviance),NA,gbms[[i]][[j]]$cv.deviance)
    )
  }
}



#Find all item in workspace that contain "gbm_tc"
#train.all<-ls(pattern="gbm_tc")

#cbind each list that contains "gbm_tc"
#train.results<-list(do.call(cbind,mget(train.all)))

#Place in a data frame
#train.results<- do.call(rbind, lapply(train.results, rbind))
#train.results <- data.frame(matrix(unlist(train.results),ncol=7 , byrow=T))

#Change column names
#colnames(train.results)<-c("TC","LR","n.trees", "AUC", "cv.AUC", "dev", "cv.dev")

#Round 4:7 down to 3 digits
train.results.par[,6:9] <- round(train.results.par[,6:9],digits=3)

#Sort by cv.dev, cv.AUC, AUC
train.results.par <- train.results.par[order(train.results.par$cv.deviance,-train.results.par$cv.AUC, -train.results.par$AUC),]

# Results deviate when using %do% and %dopar%
train.results.par #Includes a dataframe with ordered (numbered) choice based on AUC cv.dev and cv.AUC, be aware that there are mutiple ways of judging the models...

# For Agder with 378 rows in data.table
#tc     lr interaction.depth shrinkage n.trees    AUC  cv.AUC   deviance
#45  9 0.0010                 9    0.0010    4000 1.000  0.941    0.129
#30  6 0.0010                 6    0.0010    5200 1.000  0.937    0.144
#18  4 0.0050                 4    0.0050    1500 0.999  0.937    0.156
#20  4 0.0010                 4    0.0010    7200 0.998  0.939    0.163

# Use best parametrization from train.results

# brt_mod<-gbm.fixed(data=analyse.df, gbm.x = c( "distance_to_road_log", "dist_to_closest_pop_log","SCI","minimumElevationInMeters","buffer_5000m_population_2006" ,"area_km2_log","n_pop"), gbm.y = "introduced",family = "bernoulli",tree.complexity = 9, learning.rate = 0.001,bag.fraction = 1,n.trees=4000)
brt_mod<-gbm.step(data=analyse.df, gbm.x=c("distance_to_road_log", "dist_to_closest_pop_log", "SCI", "minimumElevationInMeters", "buffer_5000m_population_2006" ,"area_km2_log", "n_pop"), gbm.y="introduced", family="bernoulli", tree.complexity=9, step.size=50, learning.rate=0.001, n.trees=1000, max.trees=7000)
names(brt_mod$gbm.call)[1] <- "dataframe"

predictors<-gbm.simplify(brt_mod,n.folds = 10, n.drops = "auto", alpha = 1, prev.stratify = TRUE,
                         eval.data = NULL, plot = TRUE)

# save modell object as .rds
saveRDS(brt_mod,paste0(simdir, "/brt_mod_agder_",focal_species_var,".rds"))
