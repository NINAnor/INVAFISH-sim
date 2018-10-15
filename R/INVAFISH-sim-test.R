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

# From git_wrangle.R
# Warning message for NULL in endDate
# git_wrangle.R needs a code review with regards to NULL handling in dates
outdata_list <- wrangle_and_slice(inndata=inndata,start_year,end_year,focal_species,geoselect_native)

inndata_timeslot <- outdata_list$data

# Run git_access_connectivity_matrix2.R


# Get lake environment data
#add species to new loc_env dataframe based on outdata_data_gjedde
lake_env <- get_lake_environment(con, wbid_wrid$waterBodyID)

lake_env$Esox_lucius <- inndata_timeslot$Esox_lucius[match(as.numeric(lake_env$waterBodyID), inndata_timeslot$waterBodyID)]
lake_env$Esox_lucius[is.na(lake_env$Esox_lucius)] <- 0


# From get_lake_environment
