# Før
      # # get vector of wbID for upstream lakes 
      # upstream_lakes <- connectivity$upstream_lakes[connectivity$waterBodyID %in% introduction_lakes]      
      # upstream_lakes <- paste(upstream_lakes,sep=",",collapse=",")
      # upstream_lakes <- as.numeric(unlist(strsplit(upstream_lakes,split=",")))
      # # get vector of upstream slopes 
      # upstream_slopes <- connectivity$upstream_lakes_slope_max_max[connectivity$waterBodyID %in% introduction_lakes]
      # upstream_slopes <- paste(upstream_slopes,sep=",",collapse=",")
      # upstream_slopes <- unlist(strsplit(upstream_slopes,split=","))
      # upstream_slopes <- gsub(" ","",upstream_slopes)
      # upstream_slopes <- as.numeric(upstream_slopes)
      
      # # finally reachable lakes and assign introduction 
      # upstream_lakes_reachable <- na.omit(upstream_lakes[upstream_slopes<slope_barrier])
      # upstream_lakes_reachable <- unique(upstream_lakes_reachable) # introductions only listed once (remove duplicated lakeIDs)

      # SELECT
        # *
      # FROM 
        # temporary_agder_connectivity.lake_connectivity_summary WHERE 

        
####################################################################################
# Sette opp en test case

slope_barrirer <- 0.01

library(RPostgreSQL)
library(pool)

#Set connection parameters
pg_drv <- RPostgreSQL::PostgreSQL()
pg_host <- "vm-srv-finstad.vm.ntnu.no"
pg_db <- 'nofa'
pg_user <- rstudioapi::askForPassword("enter username")
pg_password <- rstudioapi::askForPassword("enter psw")


pool <- dbPool(
  drv = pg_drv,
  dbname = pg_db,
  host = pg_host,
  user = pg_user,
  password = pg_password,
  idleTimeout = 36000000
)

con <- poolCheckout(pool)

####################################################################################
# Nå

### Hent ut alle vannregioner (for Agder (mange inneholder ingen innsjøer)
get_wrids <- function(db_conection) {
sql_string <- "SELECT DISTINCT ON (a.gid) a.gid AS wrid FROM \"Hydrography\".\"waterregions_dem_10m_nosefi\" AS a, (SELECT geom FROM \"AdministrativeUnits\".\"Fenoscandia_Municipality_polygon\" WHERE county IN ('Vest-Agder', 'Aust-Agder')) AS b WHERE ST_Intersects(a.geom, b.geom);"
res <- dbGetQuery(db_conection, sql_string)[,1]
res
}
# Eksempel
wrids <- get_wrids(con)

### Hent ut data frame med kombinasjon av waterbodyID for alle innsjøer (kolonne 1) og id for vannregioner (kolonne 2) (her er det kun vannregioner som inneholder innsjøer)
get_wbid_wrid <- function(db_conection, eb_waterregionID) {
sql_string <- paste("SELECT id AS \"waterBodyID\", ecco_biwa_wr AS wrid FROM nofa.lake WHERE ecco_biwa_wr IN (", toString(eb_waterregionID, sep=','), ")", sep='')
res <- dbGetQuery(db_conection, sql_string)
res
}
# Eksempel
wbid_wrid <- get_wbid_wrid(con, wrids)

### Hent ut data frame med unike kombinasjon av waterbodyID (kolonne 1) og id for innsjøer som ligger nedstrøms (kolonne 2)
get_downstream_lakes <- function(db_conection, waterbodyID, eb_waterregionID) {
sql_string <- paste("SET constraint_exclusion = on;
SELECT \"lakeID\" AS  \"waterBodyID\", CAST(unnest(string_to_array(downstream_lakes, ',')) AS integer) AS downstream_lakes FROM
    temporary_agder_connectivity.lake_connectivity_summary WHERE 
    wrid IN (", eb_waterregionID,") AND
     \"lakeID\" in (", toString(waterbodyID, sep=','),");", sep='')
res <- dbGetQuery(db_conection, sql_string)
res
}
# Eksempel
downstream_lakes <- get_downstream_lakes(con, unique(wbid_wrid[,1][1:100]), unique(wbid_wrid[,2][1:100]))


### Hent ut data frame med kombinasjon av waterbodyID (kolonne 1) og id for innsjøer som ligger oppstøms og der skråning i forbindelsen er lavere enn slope_barrier (kolonne 2)
get_reachable_upstream_lakes <- function(db_conection, waterbodyID, eb_waterregionID, slope_barrirer) {
sql_string <- paste("SET constraint_exclusion = on;
SELECT
    from_lake AS source_lake, to_lake AS upstream_lake
FROM
    temporary_agder_connectivity.lake_connectivity
WHERE
    wrid in (", toString(eb_waterregionID, sep=','), ") AND
    from_lake in (", toString(waterbodyID, sep=','), ") AND
    upstream_slope_max_max <= ", slope_barrirer, "
UNION ALL SELECT
    to_lake AS source_lake, from_lake AS upstream_lake
FROM
    temporary_agder_connectivity.lake_connectivity
WHERE
    wrid in (", toString(eb_waterregionID, sep=','),") AND
    to_lake in (", toString(waterbodyID, sep=','),") AND
    downstream_slope_max_max <= ", slope_barrirer, ";", sep='')
res <- dbGetQuery(db_conection, sql_string)
res
}
# Eksempel
upstream_lakes <- get_reachable_upstream_lakes(con, unique(wbid_wrid[,1][1:100]), unique(wbid_wrid[,2][1:100]), slope_barrirer)


####################################################################################
# Rydde opp
poolReturn(con)
poolClose(pool)
