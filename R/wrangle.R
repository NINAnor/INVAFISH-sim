#############################################################
#
# Functions for data-wrangling
#
############################################################

#' Wrangle input data for format required by simulation
#'
#' @param inndata data-frame with inndata comming from "get_inndata"
#' @param start_year Year of staring recording as introductions
#' @param end_year Year of ending recording as introductions
#' @param geoselect_native A spatial.polygon.dataframe indicating the native distribution range of the species in epsg:4326
#' @param focal_species The latin (canonical) name of the species
#' @return A data.frame
#' @examples
#'
#' inndata <- readRDS("./testdata/view_occurrence_by_event.rds")
#' geoselect_native <- readRDS("./testdata/geoselect_native_Esox_lucius.rds")
#' focal_species <- "Esox lucius"
#' wrangle_and_slice(inndata=inndata,start_year=1970,end_year=2017,)
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import lubridate
#' @import data.table
#' @import FNN
#' @import sp
#'
#' @export
#'
#'

f_wrangle_and_slice <- function(start_year,end_year,inndata,focal_species,geoselect_native){

  #######################################################
  # Reclassify establishMeans of focal species
  # based upon distr. polygon
  #######################################################
  inndata_species <- inndata[inndata$scientificName==focal_species,]

  # load spatial filter (polygon) and set projection to epsg:4326
  latlong = "+init=epsg:4326"
  geoselect_spdf <- spTransform(geoselect_native,latlong)

  # convert inndata to SpatialPointDataFrame
  #inndata<-outdata_timeslot
  inndata <- as.data.frame(inndata)
  coordinates(inndata) = ~decimalLongitude + decimalLatitude

  # project outdata_event
  latlong = "+init=epsg:4326"
  proj4string(inndata) = CRS(latlong)

  # filter out values from outdata_event based upon geoselect_spdf
  # Note: clipping spatial data in R by using [] from http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
  inndata2 <- inndata[geoselect_spdf, ]

  inndata$establishmentMeans <- ifelse(inndata$waterBodyID %in% inndata2@data$waterBodyID & inndata$scientificName==focal_species,"native",
                                       "introduced")
  inndata <- as.data.fame(inndata)

  #######################################################
  # wrangle data to wide format by species
  #######################################################
  inndata <- inndata@data %>% mutate(scientificName2=str_replace(scientificName, " ", "_"))
  inndata$year <- year(parse_date_time(inndata$dateEnd,"ymd"))
  inndata <- inndata[complete.cases(inndata$year),]

  spread_data <- inndata %>% dplyr::select(eventID,scientificName2)
  spread_data$presence <- 1
  spread_data <- spread_data %>% spread(scientificName2,presence,fill=0)

  # select variables to be included. NB! No occurrence level variables, only event level and above
  inndata2 <- inndata %>% dplyr::select(dateEnd,year,
                                        recordedBy,samplingTaxaRange,samplingProtocol,
                                        datasetID,county,municipality,countryCode,eventID,#decimalLatitude,decimalLongitude,
                                        utm_x,utm_y,waterBodyID,area_km2,distance_to_road,
                                        perimeter_m,elevation,eurolst_bio01,eurolst_bio02,
                                        eurolst_bio03,eurolst_bio05,eurolst_bio06,eurolst_bio07,
                                        eurolst_bio10,eurolst_bio11) %>%
    distinct()
  inndata_event <- left_join(inndata2,spread_data,by="eventID")

  #######################################
  # Make shore line complexity variable #
  #######################################
  #make SCI, and index portraying shore line complexity (see Voje etal. 2013 for similar approach)

  inndata_event$SCI<-(inndata_event$perimeter_m/1000)/(2*sqrt(pi*inndata_event$area_km2))

  #######################################################
  # Slice by timeslot and stack output
  #######################################################

  # time-periods (timeslot)for which to loop over defined by "timeslot".
  # start year defined in input variable "startyear"
  # species defined in input variable "species

  # create two vectors of max and min year of timeslot
  n_timeslot <-  round((max(inndata$year) - startyear)/timeslot,0) # define number of timeslots
  t_slot_start <- max(inndata$year)-(timeslot*seq(1:n_timeslot))
  t_slot_end <- max(inndata$year)-(timeslot*seq(1:n_timeslot))+timeslot


  # start loooooping.....
  for (j in 1:length(species)){
    for (i in 1:length(t_slot_start)){

      # Select locations of all populations of focal species at start of timeslot i.
      # Used for calcualation distance to closest population
      data1 <- inndata %>%
        filter(year<=t_slot_start[i],scientificName==species[j]) %>%
        dplyr::select(utm_x,utm_y,waterBodyID) %>% distinct()

      # Select all locations for which there where observations of
      # fish at the end of time-slot i.
      data2 <- inndata %>%
        filter(year<=t_slot_end[i]) %>%
        dplyr::select(utm_x,utm_y,waterBodyID) %>% distinct()

      # Calculate distance to closest population of focal species -------
      # at start of time-slot for all locations with fish observations at end
      # of time-slot i. Use get.knnx from the FNN package.
      nn <- get.knnx(data1[c("utm_x","utm_y")],data2[c("utm_x","utm_y")],2)

      dist_to_closest_pop <- ifelse(nn$nn.dist[,1]==0,nn$nn.dist[,2],nn$nn.dist[,1])
      waterBodyID <- data2$waterBodyID
      distance_data <- as.data.frame(cbind(dist_to_closest_pop,waterBodyID))

      # create occurrence data of fish j in time-slot i ----
      # i.e. populations for which there are observation of fish at end of timeslot i).
      # Merge inn distance to closest population of focal species
      outdata_temp <- inndata_event %>% filter(year<=t_slot_end[i])
      outdata_temp <- left_join(outdata_temp,distance_data,by="waterBodyID")
      outdata_temp$focal_species <- species[j]
      outdata_temp$t_slot_end <- t_slot_end[i]
      outdata_temp$t_slot_start <- t_slot_start[i]
      outdata_temp$t_slot <- i

      # Add 0/1 column of introductions occuring during timeslot --------.
      # In order for this toclassify as an introduction event,
      # we first need to check out that the species not have been
      # observed in the given waterbody before.

      wb_fish_j <- inndata %>% filter(scientificName==species[j],year<=t_slot_start[i]) %>%
        dplyr::select(waterBodyID)

      # Then, select events with observation of focal species in time-slot,
      # where focal species are classified as introduced.
      outdata_temp2 <- inndata %>% dplyr::filter(scientificName==species[j],
                                                 year<=t_slot_end[i],
                                                 year>=t_slot_start[i],
                                                 establishmentMeans=="introduced",
                                                 !waterBodyID %in% wb_fish_j$waterBodyID) %>%
        dplyr::select(eventID)
      # Then classify events as introduction events of species j.
      outdata_temp$introduced <- ifelse(outdata_temp$eventID %in% outdata_temp2$eventID,
                                        1,0)

      # append above time-slot data to outdata,
      # if outdata does not exist (i.e. j=1,i=1), create outdata on the fly
      if (!exists("outdata_event")){
        outdata_event <- outdata_temp
      } else {
        outdata_event <- bind_rows(outdata_event,outdata_temp)
      }

    } # end of i loop
  } # end of j loop


  #########################################################################################
  # Group by time-slot, waterBodyID and species -----
  # output is time-slot averaged data (aggregating individual events)
  #######################################################################################

  outdata_timeslot <- outdata_event %>%
    group_by(focal_species,t_slot,t_slot_start,t_slot_end,waterBodyID,
             county,municipality,countryCode) %>%
    summarise(introduced=max(introduced),
              dist_to_closest_pop=mean(dist_to_closest_pop))

  # get presence/absence aggregated for each time-slot,waterBody and focal species
  # and join to outdata_timeslot

  # get name vector of species presence/absence variables and selectd on outdata_event
  specieslist <-as.character(names(spread_data)[2:length(names(spread_data))])
  specieslist2 <- specieslist[specieslist %in% names(outdata_event)]

  tmp <- outdata_event[, c("focal_species","t_slot","SCI","area_km2",
                           "distance_to_road","elevation","waterBodyID",
                           "decimalLatitude","decimalLongitude","utm_x","utm_y",
                           "eurolst_bio01","eurolst_bio10","eurolst_bio11",
                           specieslist2)]
  aggdata2<- tmp %>% group_by(focal_species,t_slot,waterBodyID) %>% summarise_all(funs(max))
  outdata_timeslot <- left_join(outdata_timeslot,aggdata2,
                                by = c("focal_species", "t_slot", "waterBodyID"))

  # remove all presence/absence data for species not recorded in selected area
  # (to clean up outdata)

  colsToRemove <- specieslist2[colSums(outdata_timeslot[specieslist2])==0]
  outdata_timeslot <- outdata_timeslot[!names(outdata_timeslot) %in% colsToRemove]

  # transform some variables from model
  outdata_timeslot$area_km2_log<-log(outdata_timeslot$area_km2)
  outdata_timeslot$t_slot<-as.factor(outdata_timeslot$t_slot)
  outdata_timeslot$introduced<-as.integer(outdata_timeslot$introduced)
  outdata_timeslot$dist_to_closest_pop_log<-log(outdata_timeslot$dist_to_closest_pop)
  outdata_timeslot$county<-as.factor(outdata_timeslot$county)
  outdata_timeslot$distance_to_road<-outdata_timeslot$distance_to_road+0.1
  outdata_timeslot$distance_to_road_log<-log(outdata_timeslot$distance_to_road)

  outdata <- list()
  outdata[[1]] <- outdata_event
  outdata[[2]] <- outdata_timeslot

  # write outdata to file as RDS object
  saveRDS(outdata_timeslot,"./Data/outdata_timeslot.rds")
  saveRDS(outdata_event,"./Data/outdata_event.rds")

  return(outdata)

} # end of function

