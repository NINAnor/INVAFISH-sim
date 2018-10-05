library(postGIStools)
library(RPostgreSQL)
#Set connection parameters
pg_drv <- dbDriver("PostgreSQL")
pg_user <- rstudioapi::askForPassword("enter username")
pg_password <- rstudioapi::askForPassword("enter psw")
pg_host <- "vm-srv-wallace.vm.ntnu.no"
pg_db<-"nofa"

#Initialise connection
con<-dbConnect(pg_drv,dbname=pg_db,user=pg_user, password=pg_password,host=pg_host)

geoselect_native_gjedde <- get_postgis_query(con, 'SELECT ST_Transform(geom, 4326) AS geom, ogc_fid, occurrence  FROM public.gjedde ',  geom_name = "geom")
eoselect_native_mort <- get_postgis_query(con, 'SELECT ST_Transform(geom, 4326) AS geom, ogc_fid, occurrence  FROM temporary.mort ',  geom_name = "geom")
geoselect_native_soerv <- get_postgis_query(con, 'SELECT ST_Transform(geom, 4326) AS geom, ogc_fid, occurrence  FROM temporary.soerv ',  geom_name = "geom")
geoselect_native_oerekyte <- get_postgis_query(con, 'SELECT ST_Transform(geom, 4326) AS geom, ogc_fid, occurrence  FROM temporary.oerekyte ', geom_name = "geom")


geoselect_no_gjedde_pop_5000 <- dbGetQuery(con, 'SELECT al.id AS "waterBodyID", count(ol.geom) FROM
                                                  nofa.lake AS al,
                                           (SELECT geom FROM nofa.lake WHERE id IN (SELECT "waterBodyID" FROM nofa.get_last_occurrence_status(
                                           "taxonID" => 26181,
                                           counties => \'Vest-Agder,Aust-Agder\'))) AS ol
                                           WHERE ST_DWithin(al.geom, ol.geom, 5000)
                                           GROUP BY al.id')


                                                  