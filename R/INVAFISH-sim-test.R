if (!require('devtools', character.only=T, quietly=T)) {
  # Requires OpenSSL on the system
  install.packages('devtools')
  require('devtools')
}

if (!require('easypackages', character.only=T, quietly=T)) {
  devtools::install_github("jakesherman/easypackages")
  require('easypackages')
  }

library(easypackages)

req_packages <- c('dplyr', 'getPass', 'dbplyr', 'lubridate', 'FNN', 'stringr', 'gbm', 'dismo', 'doParallel', 'RPostgreSQL', 'pool', 'postGIStools')

for (p in req_packages) {
  if(!require(p, character.only=T, quietly=T)){
    install.packages(p)
    print(p)
  }
}
libraries(req_packages)

scriptdir <- '~/INVAFISH-sim/'
setwd(scriptdir)

simdir <- '~/temp/'
try(system(paste0('mkdir ', simdir)))

focal_species <- "Esox lucius"
focal_speciesid <- 26181
focal_species_str <- gsub(" ", "_", focal_species)
start_year <- 1967
end_year <- 2017


conmat_schema <- "fremmedfisk"
conmat_table <- "fremmedfisk_lake_connectivity"
conmat_summary_table <- "fremmedfisk_lake_connectivity_summary"

### Setup database connection
#Set connection parameters
pg_drv <- RPostgreSQL::PostgreSQL()
pg_db <- 'nofa'
host_msg <- 'Please enter host name:'
user_msg <- 'Please enter your user name:'
pw_msg <- "Please enter your password:"
if (rstudioapi::isAvailable()) {
  pg_host <- rstudioapi::showPrompt(title='Hostname', message=host_msg, default='')
  pg_user <- rstudioapi::showPrompt(title='Username', message=user_msg, default='')
  pg_password <- rstudioapi::askForPassword(prompt=pw_msg)
} else {
  pg_host <- getPass(msg=host_msg)
  pg_user <- getPass(msg=user_msg)
  pg_password <- getPass(msg=pw_msg)
}


pool <- dbPool(
  drv = pg_drv,
  dbname = pg_db,
  host = pg_host,
  user = pg_user,
  password = pg_password,
  idleTimeout = 36000000
)

con <- poolCheckout(pool)


### Hent ut alle vannregioner fra konnektivitetsmatrisa
# Run git_access_connectivity_matrix2.R
source('./R/git_access_connectivity_matrix2.R')
wbid_wrid <- get_wbid_wrid(con, "fremmedfisk", "fremmedfisk_lake_connectivity_summary")

# From dataIO.R
source('./R/dataIO.R')
get_inndata(serveradress=pg_host, datafolder=simdir)
inndata <- readRDS(paste0(simdir, "view_occurrence_by_event.rds", sep=''))


# From get_geoselect_native.R
source('./R/get_geoselect_native.R')
geoselect_native <- get_historic_distribution(con, focal_speciesid)
geoselect_no_gjedde_pop_5000 <- dbGetQuery(con, paste0('SELECT al.id AS "waterBodyID", count(ol.geom) FROM
                                                  nofa.lake AS al,
                                           (SELECT geom FROM nofa.lake WHERE id IN (SELECT "waterBodyID" FROM nofa.get_last_occurrence_status(
                                           "taxonID" => ', focal_speciesid, ',
                                           wrids => \'', gsub(" ", "", toString(unique(wbid_wrid$wrid))), '\'
                                           ))) AS ol
                                           WHERE ST_DWithin(al.geom, ol.geom, 5000)
                                           GROUP BY al.id'))
names(geoselect_no_gjedde_pop_5000)[2] = "n_pop"

inndata <- merge(inndata, geoselect_no_gjedde_pop_5000, all.x=TRUE)
inndata["n_pop"][is.na(inndata["n_pop"])] <- 0
#paste0("SELECT * FROM nofa.number_populations_5000m_pike WHERE \"waterBodyID\" IN (",toString(),")")

# From git_wrangle.R
# Warning message for NULL in endDate
# git_wrangle.R needs a code review with regards to NULL handling in dates
source('./R/git_wrangle.R')
outdata_list <- wrangle_and_slice(inndata=inndata,start_year,end_year,focal_species,geoselect_native)

inndata_timeslot <- outdata_list$data

# Get lake environment data from get_lake_environment.R
#add species to new loc_env dataframe based on outdata_data_gjedde
source('./R/get_lake_environment.R')
lake_env <- get_lake_environment(con, wbid_wrid$waterBodyID)

lake_env[focal_species_str] <- inndata_timeslot[focal_species_str][match(as.numeric(lake_env$waterBodyID), inndata_timeslot$waterBodyID),]
lake_env[focal_species_str][is.na(lake_env[focal_species_str])] <- 0

lake_env$introduced <- inndata_timeslot$introduced[match(as.numeric(lake_env$waterBodyID), inndata_timeslot$waterBodyID)]
lake_env$introduced[is.na(lake_env$introduced)] <- 0

lake_env <- merge(lake_env, geoselect_no_gjedde_pop_5000, by="waterBodyID", all.x=TRUE)
lake_env$n_pop <- lake_env$n_pop - 1
lake_env$n_pop <- ifelse(is.na(lake_env$n_pop), 0, lake_env$n_pop)

#add recalculated closest distance based on new data
source('./R/f_calc_distance.R')
a <- f_calc_dist(outdata=lake_env,species=focal_species)
lake_env$dist_to_closest_pop_log <- log(a$dist_to_closest_pop)


# Get model from git_make_brt.R
source('./R/git_make_brt.R')


#.........................................................................................
# Define input variables ----
#.........................................................................................

# Select inndata geographic range based upon connectivity matrix extent
#inndata <- loc_env[loc_env$waterBodyID %in% wbid_wrid$waterBodyID,]


temp_inc <- 0 # temperature increas

# simulation and time specific stuff
Nsims <- 300 # number of iterations
sim_duration <- 1 # Duration of the scenario, in years (will be corced to multiple of time_slot_length)
time_slot_length <- 50 # Duration of the time-slot, in years (time-span of each time-slot)
gmb_time_slot_length <- 50 # Duration of the time-slot, in years, used to estimate establishment probability
n_time_slots <- 2#as.integer(sim_duration/time_slot_length)
start_year_sim <- 2017
end_year_sim <- start_year_sim + time_slot_length
# secondary dispersal stuff
with_secondary <- TRUE # should secondary spread be included in simulations?
slope_barrier <- 700 # max slope for migration upstream
percentage_exter <- 0.0 # Give the percentage of focal species populations one wants to exterminate before simulation

# Set on or the other to true or false (for upstream dispersal). Probability is based on analyses from Sam and Stefan
use_slope_barrier<-TRUE
use_disp_probability<-FALSE

# Before each simulation run!!!!
# Create new dataframe / vars for simulation bookkeeping.
# Use latest time-slot (if multiple) in inndata
inndata_sim1 <- lake_env#[inndata$t_slot==unique(inndata$t_slot)[1],]

#inndata_sim1<-as.data.frame(inndata_sim1)
# Exterminate prosentage of present populations of focal species.


if (percentage_exter > 0 & percentage_exter < 1.0) {
inndata_sim1[ sample( which(inndata_sim1[focal_species_str]==1), round(percentage_exter*length(which(inndata_sim1[focal_species_str]==1)))), ][focal_species_str] <- 0
} else if (percentage_exter == 1.0) {
# Exterminate all pressent populations in VFO Trondelag....
inndata_sim1[focal_species_str] <- 0
}

#exterminate specific lakes
exwaterbodyID <- NA # c(2646158,3530010,2701067,3521235,2833551)

if(!is.na(exwaterbodyID)) {
inndata_sim1[focal_species_str][inndata_sim1$waterBodyID %in% exwaterbodyID ] <- 0
}

source('./R/git_predict_introduction_events.R')
brt_mod <-brt_mod_simp

# j simulation runs...
for(j in 1:Nsims){

  inndata_sim <- as.data.frame(inndata_sim1) # reset dataset for each simulation run

  # simulate across time periods j
  # to run in parallelization, see: https://www.r-bloggers.com/parallel-r-loops-for-windows-and-linux/
  for(i in 1:n_time_slots){

    ### i.1 predict translocations and store new introductions in temp object
    tmp_trans <- f_predict_introduction_events_gmb(inndata_sim,brt_mod,focal_species,temp_inc,start_year_sim)
    tmp_trans <- tmp_trans[!is.na(tmp_trans[focal_species_str]),]
    # include secondary dispeersal?
    if(with_secondary==TRUE){

      # get wbID from introductions in run i
      introduction_lakes <- tmp_trans[tmp_trans$introduced==1,]$waterBodyID
      species_lakes <- tmp_trans$waterBodyID[tmp_trans[focal_species_str]==1]
      introduction_wrid <- wbid_wrid$wrid[wbid_wrid$waterBodyID %in% species_lakes]
      species_wrid_wbid <- wbid_wrid[wbid_wrid$waterBodyID %in% species_lakes,]
      disperse_input <- species_wrid_wbid %>%
           group_by(wrid) %>%
           summarise(waterBodyIDs = toString(waterBodyID))

      introduction_lakes <- wbid_wrid$waterBodyID[wbid_wrid$waterBodyID %in% species_lakes]
      #assign new introductions
      tmp_trans$introduced <- ifelse(tmp_trans$waterBodyID %in% introduction_lakes,1,tmp_trans$introduced)
      # introduction_lakes[!is.na(introduction_lakes)]
      #.............................................................
      # Downstream dispersal
      #.............................................................
      # get vector of wbID for downstream lakes to those with introduction in simrun i
      #downstream_lakes <- connectivity$downstream_lakes[connectivity$waterBodyID %in% introduction_lakes]
      #downstream_lakes <- paste(downstream_lakes,sep=",",collapse=",")
      #downstream_lakes <- na.omit(as.numeric(unlist(strsplit(downstream_lakes,split=","))))
      #downstream_lakes <- unique(downstream_lakes) # introductions only listed once (remove duplicated lakeIDs)

      # select out downstream lakes that does not have species at start of time-slot
      if(use_slope_barrier==TRUE){
        # wbid_wrid_array <- get_wbid_wrid_array(con, species_lakes)
        reachable_lakes_species <- get_reachable_lakes(con, disperse_input$wrid, disperse_input$waterBodyIDs, "slope_max_max", slope_barrier, conmat_schema, conmat_table)

        #downstream_lakes <- get_downstream_lakes(con, unique(introduction_lakes), unique(introduction_wrid))
        # select out downstream lakes that does not have species at start of time-slot
        #downstream_lakes <- downstream_lakes[!(downstream_lakes$downstream_lakes %in% inndata_sim$waterBodyID[inndata_sim[focal_species_str]==1]),]
        reachable_lakes_species <- setdiff(reachable_lakes_species[,1], inndata_sim$waterBodyID[inndata_sim[focal_species_str]==1])
        # finally assign introduction to downstream lakes (without previous obs/intro)

        tmp_trans$introduced <- ifelse(tmp_trans$waterBodyID %in% reachable_lakes_species,1,tmp_trans$introduced)

        #.............................................................
        # Upstream dispersal - NB! Check this part.... unequal length of upstream_lakes and upstream_slopes vector!!!!
        #.............................................................
        # get vector of wbID for upstream lakes
        #upstream_lakes <- connectivity$upstream_lakes[connectivity$waterBodyID %in% introduction_lakes]
        #upstream_lakes <- paste(upstream_lakes,sep=",",collapse=",")
        #upstream_lakes <- as.numeric(unlist(strsplit(upstream_lakes,split=",")))
        # get vector of upstream slopes
        #upstream_slopes <- connectivity$upstream_lakes_slope_max_max[connectivity$waterBodyID %in% introduction_lakes]
        #upstream_slopes <- paste(upstream_slopes,sep=",",collapse=",")
        #upstream_slopes <- unlist(strsplit(upstream_slopes,split=","))
        #upstream_slopes <- gsub(" ","",upstream_slopes)
        #upstream_slopes <- as.numeric(upstream_slopes)

        # finally reachable lakes and assign introduction
        #upstream_lakes_reachable <- na.omit(upstream_lakes[upstream_slopes<slope_barrier])
        #upstream_lakes_reachable <- unique(upstream_lakes_reachable) # introductions only listed once (remove duplicated lakeIDs)

        ##Use a predefined slope barrier...

        #upstream_lakes_reachable <- get_reachable_upstream_lakes(con, unique(introduction_lakes), unique(introduction_wrid), slope_barrier)
        # select out upstream lakes that does not have species at start of time-slot
        #upstream_lakes_reachable <- upstream_lakes_reachable[!(upstream_lakes_reachable$upstream_lake %in% inndata_sim$waterBodyID[inndata_sim[focal_species_str]==1]),]

        # add upstream_lake intros to introduced vector
        #tmp_trans$introduced <- ifelse(tmp_trans$waterBodyID %in% upstream_lakes_reachable$upstream_lake,1,tmp_trans$introduced)
      }
      ##..or dispersal probability based on analyses from Sam and Stefan
      if(use_disp_probability==TRUE){
        lakes_reachable <-get_reachable_lakes_species(con,unique(introduction_lakes))
        # select out upstream lakes that does not have species at start of time-slot
        lakes_reachable <- lakes_reachable[!(lakes_reachable$accessible_lake %in% inndata_sim$waterBodyID[inndata_sim[focal_species_str]==1]),]
        # add lake intros to introduced vector, based on probability
        tmp_trans$introduced <- ifelse(tmp_trans$waterBodyID %in% lakes_reachable$accessible_lake,rbinom(length(tmp_trans$waterBodyID), size = 1, prob=lakes_reachable$likelihood),tmp_trans$introduced)
      }

    } # end of secondary==TRUE if statement

    ### Store output from time-period i, simulationrun j

    # create variables to store
    intro <- tmp_trans$waterBodyID[tmp_trans$introduced==1]
    intro_is_secondary <-0
    if(with_secondary==TRUE){
    intro_is_secondary <- ifelse(intro %in% reachable_lakes_species,TRUE,FALSE)
    }
    time_slot_i <- rep(i,length(intro))
    sim_j <- rep(j,length(intro))
    start_year_i <- rep(( start_year+((i-1)*time_slot_length) ),
                        length(intro))
    end_year_i <- rep(( start_year+(i*time_slot_length)-1 ),
                      length(intro))
    tmp_output <- data.frame(intro=intro,
                             intro_is_secondary=intro_is_secondary,
                             time_slot_i=time_slot_i,
                             sim_j=sim_j,
                             start_year_i=start_year_i,
                             end_year_i=end_year_i)

    # on first sim-run, create new object to store output
    if(i==1 & j==1){
      sim_output <- data.frame()
    }
    # append sim_output with data from time-slot i, sim j
    sim_output <- bind_rows(sim_output,tmp_output)

    ### modify inndata_sim with new introductions to provide innput to time_slot i+1
    # Add new introductions to "focal_species_str" column in inndata_sim
    tmp1 <- inndata_sim[focal_species_str]
    inndata_sim[focal_species_str][inndata_sim$waterBodyID %in% tmp_trans$waterBodyID[tmp_trans$introduced==1],] <- 1


    # n_pop should be recalculated as well!!!


    # recalculate distance to closest population and replace values
    # in inndata_sim where distance is smaller than previous;
    # i.e. accounting for situations where closest population is outside
    # the geographic area beein investigated
    # NB! need a try function here to make run in cases where there are non intros in simrun
    # and f_calc_dist fail
    tmp <- try(f_calc_dist(outdata=inndata_sim,species=focal_species),silent = TRUE)
    if(!is.null(dim(tmp))){
      inndata_sim$dist_to_closest_pop_log <- ifelse(log(tmp$dist_to_closest_pop)<inndata_sim$dist_to_closest_pop,
                                                log(tmp$dist_to_closest_pop),
                                                inndata_sim$dist_to_closest_pop_log)
    } else {
      inndata_sim$dist_to_closest_pop_log <-  inndata_sim$dist_to_closest_pop_log
    }

    tmp <- data.frame()

    # and log variable for model predictions
    # inndata_sim$dist_to_closest_pop_log <- log(inndata_sim$dist_to_closest_pop)

  } # end of i loop

  # display progress
  print(paste("sim-run: ",j,", out of: ",Nsims))

} # end of j loop

#............................................................................
# sum up sim_output pr lake and save / write to database
#............................................................................
# store inndata1, raw and lake aggregated sim_output as list in rds object
source('./R/f_postsim_processing.R')
sim_output_lake <- f_sim_output_lake(sim_output,inndata_sim1,Nsims)


tmpout <- list()
tmpout[["sim_output"]] <- sim_output
tmpout[["inndata_sim1"]] <- inndata_sim1
tmpout[["sim_output_lake"]] <- sim_output_lake
tmpout[["focal_species"]] <- species
tmpout[["time_slot_length"]] <- time_slot_length
tmpout[["start_year"]] <- start_year

# Write output to local disk
url <- paste(simdir, "sim_out_",focal_species_str,"_telemark_300simu.rds",sep="")
saveRDS(tmpout,url)

# write output to BOX
#library(boxr)
#box_auth() # When run on server, create OAuth2.0 token locally and copy to server - see boxr vignett (remember .gitignore if needed)
#box_ul(dir_id = 29103527607, file = paste("./Data/sim_out_",focal_species_str,".rds",sep=""))

# Write lake-specific summary to database
dataToWrite <- sim_output_lake
#nameOfTable <- tolower(paste("sim_output_",focal_species_str,"_no_extermination_scenario",sep=""))
#f_write_simresult_to_db(dataToWrite=sim_output_lake,nameOfTable)


dbWriteTable(con, c(conmat_schema, paste0("sim_out_lake_", tolower(focal_species_str) ,"_without_ext_",Nsims,"simu"), value=dataToWrite,overwrite=TRUE))

#dbWriteTable(con, c("temporary_agder", "sim_agder_output_esox_lucius"), as.data.frame(sim_output_lake))

# Rydde opp i forbindelse mot server
poolReturn(con)
poolClose(pool)
