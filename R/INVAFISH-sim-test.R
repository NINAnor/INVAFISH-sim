simdir <- '~/temp/'
pg_host <- "vm-srv-wallace.vm.ntnu.no"

focal_species <- "Esox lucius"
focal_speciesid <- 26181
start_year <- 1967
end_year <- 2017

# From dataIO.R
get_inndata(serveradress=pg_host, datafolder=simdir)
inndata <- readRDS(paste0(simdir, "view_occurrence_by_event.rds", sep=''))

# From get_geoselect_native.R
geoselect_native <- get_historic_distribution(con, focal_speciesid)
geoselect_no_gjedde_pop_5000 <- dbGetQuery(con, 'SELECT al.id AS "waterBodyID", count(ol.geom) FROM
                                                  nofa.lake AS al,
                                           (SELECT geom FROM nofa.lake WHERE id IN (SELECT "waterBodyID" FROM nofa.get_last_occurrence_status(
                                           "taxonID" => 26181,
                                           counties => \'Vest-Agder,Aust-Agder\'))) AS ol
                                           WHERE ST_DWithin(al.geom, ol.geom, 5000)
                                           GROUP BY al.id')

# From git_wrangle.R
# Warning message for NULL in endDate
# git_wrangle.R needs a code review with regards to NULL handling in dates
source('./R/git_wrangle.R')
outdata_list <- wrangle_and_slice(inndata=inndata,start_year,end_year,focal_species,geoselect_native)

inndata_timeslot <- outdata_list$data

# Run git_access_connectivity_matrix2.R
source('./R/git_access_connectivity_matrix2.R')


# Get lake environment data from get_lake_environment.R
#add species to new loc_env dataframe based on outdata_data_gjedde
source('./R/get_lake_environment.R')
lake_env <- get_lake_environment(con, wbid_wrid$waterBodyID)

lake_env$Esox_lucius <- inndata_timeslot$Esox_lucius[match(as.numeric(lake_env$waterBodyID), inndata_timeslot$waterBodyID)]
lake_env$Esox_lucius[is.na(lake_env$Esox_lucius)] <- 0

lake_env$introduced <- inndata_timeslot$introduced[match(as.numeric(lake_env$waterBodyID), inndata_timeslot$waterBodyID)]
lake_env$introduced[is.na(lake_env$introduced)] <- 0

lake_env$n_pop<-0
lake_env$n_pop<-ifelse(lake_env$waterBodyID %in% geoselect_no_gjedde_pop_5000$waterBodyID,geoselect_no_gjedde_pop_5000$count,lake_env$n_pop)

#add recalculated closest distance based on new data
a <- f_calc_dist(outdata=lake_env,species=focal_species)
lake_env$dist_to_closest_pop_log <- log(a$dist_to_closest_pop)


# Get model from git_make_brt.R
source('./R/git_make_brt.R')


#.........................................................................................
# Define input variables ----
#.........................................................................................

# Select inndata geographic range based upon connectivity matrix extent
#inndata <- loc_env[loc_env$waterBodyID %in% wbid_wrid$waterBodyID,]

# species specific stuff
species_var <- stringr::str_replace(focal_species," ","_") # variable describing present/absence of focal species

temp_inc <- 0 # temperature increas

# simulation and time specific stuff
Nsims <- 5 # number of iterations
sim_duration <- 1 # Duration of the scenario, in years (will be corced to multiple of time_slot_length)
time_slot_length <- 50 # Duration of the time-slot, in years (time-span of each time-slot)
gmb_time_slot_length <- 50 # Duration of the time-slot, in years, used to estimate establishment probability
n_time_slots <- 2#as.integer(sim_duration/time_slot_length)
start_year <- 2017
end_year <-2017 + time_slot_length
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
inndata_sim1[ sample( which(inndata_sim1$Esox_lucius==1), round(percentage_exter*length(which(inndata_sim1$Esox_lucius==1)))), ]$Esox_lucius <- 0
} else if (percentage_exter == 1.0) {
# Exterminate all pressent populations in VFO Trondelag....
inndata_sim1[species_var] <- 0
}
