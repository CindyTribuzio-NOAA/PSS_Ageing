# Query fishery data to look for hauls with missing data forms----
# Contact: cindy.tribuzio@noaa.gov
# Last Updated: January 2024

# Setup ----
dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

channel_akfin <- odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

#outpath <- paste0("Data/Cleaned/", AYR)
#dir.create(outpath)
rawpath <- paste0("Data/", AYR) 
dir.create(rawpath)


#SQL code, runs faster in SQL developer than R, not worth coding it in R at this time
select * from NORPAC.debriefed_offload_mv
where (permit = 4078 and cruise = 24937 and offload_number = 16)

select * from norpac.debriefed_haul
where (cruise = 2353 and permit = 1222 and haul = 28)

select * from norpac.debriefed_trip
where (vessel_name = 'WESTWARD I' and year = 2019)

select adfg_H_adfg_number, adfg_B_batch_year, adfg_H_date_landed, adfg_I_species_code, cfec_stat_area   
from council.comprehensive_ft
where (adfg_H_adfg_number = 40309 and adfg_B_batch_year = '2021' and adfg_I_species_code = 692)

select adfg_H_adfg_number, adfg_B_batch_year, adfg_H_date_landed, adfg_I_species_code, cfec_stat_area   
from council.comprehensive_ft
where (adfg_h_landing_report_number = 7939767 and adfg_B_batch_year = '2021'  and adfg_I_species_code = 692)

#this query gets your from offload to adfg stat area
select NORPAC.debriefed_offload_mv.permit, NORPAC.debriefed_offload_mv.cruise, NORPAC.debriefed_offload_mv.offload_number,NORPAC.debriefed_offload_mv.landing_report_id,
NORPAC.debriefed_offload_mv.gear_type_code, NORPAC.debriefed_offload_mv.nmfs_area, council.comprehensive_ft.adfg_H_adfg_number, 
council.comprehensive_ft.adfg_B_batch_year, council.comprehensive_ft.adfg_H_date_landed, council.comprehensive_ft.adfg_I_species_code, council.comprehensive_ft.cfec_stat_area
from norpac.debriefed_offload_mv
left join council.comprehensive_ft 
on norpac.debriefed_offload_mv.landing_report_id=COUNCIL.comprehensive_ft.adfg_h_landing_report_number
where (NORPAC.debriefed_offload_mv.permit = 5306 and NORPAC.debriefed_offload_mv.cruise = 24307 and NORPAC.debriefed_offload_mv.offload_number = 34 and
       COUNCIL.comprehensive_ft.adfg_I_species_code = 692)




# Check Haul Data ---- (this is actually faster in SQL developer, but here's the code for reference)
NORPAChaul <- sqlQuery(channel_afsc, query = ("
                select    *
                from      obsint.debriefed_haul
                where (year = 2020 and cruise = 24322 and haul = 76")) %>% 
  clean_names()

Vesselcheck <- sqlQuery(channel_afsc, query = ("
                select    *
                from      obsint.debriefed_trip
                where (year = 2020 and cruise = 24322")) %>% 
  clean_names()


