
library(readr)
library(raster)
library(tidyverse)
library(lubridate)
library(devtools)
library(remotes)
library(soilDB)
library(dplyr)
sessionInfo()
#-----------------TEXAS-------------------

#Step 1: set up Soilwat2 (skip this if already done and just load in packages)
#####
#remotes::install_github("DrylandEcology/rSOILWAT2",force=T)
library(rSOILWAT2)
library(git2r)

sw_args <- function(dir, files.in, echo, quiet) {
  input <- "SOILWAT2"
  
  if (dir != "")
    input <- c(input, "-d", dir)
  if (files.in != "")
    input <- c(input, "-f", files.in)
  if (echo)
    input <- c(input, "-e")
  if (quiet)
    input <- c(input, "-q")
  
  input
}

path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")

#TX 
site<-c("TX")
Latitude<-c(30.353845)
Longitude<-c(-104.045262)
Tree_cover<-c(0)                                                                                                                           
Shrub_cover<-c(.03)
Grass_cover<-c(.92)
Forb_cover<-c(.05)
Bare_ground<-c(.2)
Annuals_binary<-c(1)
C3_cover<-c(.1)
C4_cover<-c(.9)
Bulk_density<-c(1.45)
Gravel<-c(0)
Sand<-c(.247)
Clay<-c(.34)

TX_point<-data.frame(site,Latitude,Longitude,Tree_cover,Shrub_cover,Grass_cover,Forb_cover,Bare_ground,
                     Annuals_binary,C3_cover,C4_cover,
                     Bulk_density,Gravel,Sand,Clay)

yearStart<-1968
yearEnd<-2010


#Creating the shells for SOILWAT2 data input
path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")

# Load sw_in from the example files
sw_in <- sw_inputDataFromFiles(dir = path_demo, files.in = "files.in")  
sw_in@years@StartYear = as.integer(yearStart)
sw_in@years@EndYear = as.integer(yearEnd)

co2 = read.csv('~/Desktop/CO2_historic.csv')
sw_in@carbon@CO2ppm = as.matrix(co2)


#weather database creation for SOILWAT2 input
# Loop through each coordinate in the dataframe
for (i in 1:nrow(TX_point)) {
  #Each of these needs to be a column in your data frame
  lat <- TX_point$Latitude[i]
  lon <- TX_point$Longitude[i]
  Tree_cover<-TX_point$Tree_cover[i]
  Shrub_cover<-TX_point$Shrub_cover[i]
  Forb_cover<-TX_point$Forb_cover[i]
  Grass_cover<-TX_point$Grass_cover[i]
  Bare_ground<-TX_point$Bare_ground[i]
  Annuals_binary<-TX_point$Annuals_binary[i]
  C3_cover<-TX_point$C3_cover[i]
  C4_cover<-TX_point$C4_cover[i] #C3 + C4 need to add up to Grass_cover amount
  Bulk_density<-TX_point$Bulk_density[i]
  Gravel<-TX_point$Gravel[i]
  Sand<-TX_point$Sand[i]
  Clay<-TX_point$Clay[i]
  {
    # Download from `DayMet`
    dm_gradient <- try(daymetr::download_daymet(
      site =  paste0("TX_point", i), 
      lat = lat,
      lon = lon,
      start = 1982,
      end = 2024,
      internal = TRUE,
      simplify = FALSE
    ))
    
    if (!inherits(dm_gradient, "try-error")) {
      # Convert data to a `rSOILWAT2`-formatted weather object
      vars <- c("year", "yday", "tmax..deg.c.", "tmin..deg.c.", "prcp..mm.day.")
      xdf <- dm_gradient[["data"]][, vars]
      xdf[, "prcp..mm.day."] <- xdf[, "prcp..mm.day."] / 10 # convert mm -> cm
      colnames(xdf) <- c("Year", "DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      
      xdf$Year <- xdf$Year - (1982  - 1968)
      wdata_dm <- rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf)
      
      # Convert `DayMet`'s `noleap` calendar to proleptic Gregorian calendar
      xdf2 <- rSOILWAT2::dbW_convert_to_GregorianYears(weatherData = wdata_dm)
      
      wdata <- rSOILWAT2::dbW_generateWeather(
        weatherData = rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf2),
        seed = 123
      )
      
      # Check that weather data is well-formed
      stopifnot(rSOILWAT2::dbW_check_weatherData(wdata))
    }
    
    
    clim <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, do_C4vars = TRUE)
    #Generate data off Weekmet dataf
    veg_cover <- rSOILWAT2::estimate_PotNatVeg_composition(
      MAP_mm = 10 * clim[["MAP_cm"]],
      MAT_C = clim[["MAT_C"]],
      mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
      mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
      dailyC4vars = clim[["dailyC4vars"]]
    )
    
    #Put in your vegetation values below
    veg_cover[["Rel_Abundance_L1"]][["SW_TREES"]]<- Tree_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_SHRUB"]]<- Shrub_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_FORBS"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_GRASS"]]<-Grass_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_BAREGROUND"]]<-Bare_ground
    
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]]<-C3_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]]<-C4_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]]<-1 #Binary between 1 and 0
    veg_cover[["Rel_Abundance_L0"]][["Shrubs"]]<-Shrub_cover
    veg_cover[["Rel_Abundance_L0"]][["Forbs"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L0"]][["Trees"]]<-Tree_cover
    veg_cover[["Rel_Abundance_L0"]][["BareGround"]]<-Bare_ground
    veg_cover[["Rel_Abundance_L0"]][["Succulents"]]<-0
    
    ids <- sapply(
      X = names(rSOILWAT2::swProd_Composition(sw_in)),
      FUN = function(x) {
        grep(
          pattern = substr(x, 1, 4),
          x = names(veg_cover[["Rel_Abundance_L1"]]),
          ignore.case = TRUE
        )
      }
    )
    # Assign fractional cover values to rSOILWAT2 input object
    rSOILWAT2::swProd_Composition(sw_in) <- veg_cover[["Rel_Abundance_L1"]][ids]
    ### Biomass amount and phenology (for shrubs and grasses)
    # Reference biomass values from Bradford et al. 2014 are used
    # Mean monthly reference temperature corresponding to default phenology values
    # for the median across 898 big sagebrush sites are used
    
    
    veg_biom <- rSOILWAT2::estimate_PotNatVeg_biomass(
      target_temp = clim[["meanMonthlyTempC"]],
      target_MAP_mm = 10 * clim[["MAP_cm"]],
      do_adjust_phenology = TRUE,
      do_adjust_biomass = TRUE,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Assign monthly biomass values to rSOILWAT2 input object
    # Note: monthly biomass values of forbs, trees, etc. need to be estimated
    v1 <- c("Litter", "Biomass", "Perc.Live")
    v2 <- c("Litter", "Biomass", "Live_pct")
    rSOILWAT2::swProd_MonProd_grass(sw_in)[, v2] <- veg_biom[["grass"]][, v1]
    rSOILWAT2::swProd_MonProd_shrub(sw_in)[, v2] <- veg_biom[["shrub"]][, v1]
    
    ### Rooting profiles of vegetation types
    
    # Select rooting profile types
    # Set those to "FILL" where cover == 0 (because of transpiration regions)
    trco_type_by_veg <- list(
      grass_C3 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_C4 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_annuals = if (
        veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]] > 0
      ) {
        "Jacksonetal1996_crops"
      } else {
        "FILL"
      },
      shrub = if (veg_cover[["Rel_Abundance_L0"]][["Shrubs"]] > 0) {
        "SchenkJackson2003_PCdry_shrubs"
      } else {
        "FILL"
      },
      forb = if (veg_cover[["Rel_Abundance_L0"]][["Forbs"]] > 0) {
        "SchenkJackson2003_PCdry_forbs"
      } else {
        "FILL"
      },
      tree = if (veg_cover[["Rel_Abundance_L0"]][["Trees"]] > 0) {
        "Bradfordetal2014_LodgepolePine"
      } else {
        "FILL"
      }
    )
    
    ### Create shell soil data base:
    soil_fixed <- data.frame(
      depth_cm = c(5, 10, 20, 30, 40,60,80,100),
      bulkDensity_g.cm.3 = rep(Bulk_density, 8),
      gravel_content = rep(Gravel, 8),
      EvapBareSoil_frac = c(0.9182,0.0818,0.000,0.000,0.000,0.000,0.000,0.000),
      transpGrass_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      transpShrub_frac = c(0.1585857,0.1414536,0.2387138,0.4612469,0.4612469,0.2387138,0.4612469,0.4612469),
      transpTree_frac =  c(0.1,0.1,0.2,0.6,0.4,0.2,0.6,0.4),
      transpForb_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      sand_frac = rep(Sand, 8),
      clay_frac = rep(Clay, 8),
      impermeability_frac = c(0,0,0,0,0,0,0,0),
      soilTemp_c = c(-0.9803060,-0.9606121,-0.9212242,-0.8030605, -0.8030605,-0.9212242,-0.8030605, -0.8030605)
    )
    soil_new <- data.frame(rSOILWAT2::swSoils_Layers(sw_in)[0, ])
    
    soil_new[seq_len(nrow(soil_fixed)), ] <- soil_fixed
    
    veg_roots <- rSOILWAT2::estimate_PotNatVeg_roots(
      layers_depth = soil_new[, "depth_cm"],
      trco_type_by_veg = trco_type_by_veg,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Add rooting profile to soil
    v1 <- c("Grass", "Shrub", "Tree", "Forb")
    v2 <- paste0("transp", v1, "_frac")
    soil_new[, v2] <- veg_roots[, v1]
    # Create a new sw_in object with soil_new
    rSOILWAT2::swSoils_Layers(sw_in) <- data.matrix(soil_new)
    # Run sw_in through rSOILWAT2::sw_exec() to create sw_out
    sw_out <- rSOILWAT2::sw_exec(inputData = sw_in, weatherList = wdata)
    
    TX_output_TRANPS <- as.data.frame(sw_out@TRANSP@Day)
  }
}

transp_TX <- TX_output_TRANPS %>%
  mutate(Year = Year + 13) %>%
  rename(DOY = Day, year = Year) %>%
  dplyr::select(1:10)

transp_TX<-transp_TX%>%
  group_by(year,DOY)%>%
  mutate(total = sum(transp_total_Lyr_1,transp_total_Lyr_2,transp_total_Lyr_3,transp_total_Lyr_4,
                     transp_total_Lyr_5,transp_total_Lyr_6,transp_total_Lyr_7,transp_total_Lyr_8))


write.csv(transp_TX, "~/Desktop/Thesis Stuff/phenology/soilwatdata/transpiration/transp_TX.csv")


#---------------------------------------------NEW MEXICO--------------------------------------------------
site<-c("NM")
Latitude<-c(33.606785)
Longitude<-c(-103.3562783)
Tree_cover<-c(0)                                                                                                                           
Shrub_cover<-c(.15)
Grass_cover<-c(.83)
Forb_cover<-c(.02)
Bare_ground<-c(0)
Annuals_binary<-c(1)
C3_cover<-c(.15)
C4_cover<-c(.85)
Bulk_density<-c(1.45)
Gravel<-c(0)
Sand<-c(.947)
Clay<-c(.034)

NM_point<-data.frame(site,Latitude,Longitude,Tree_cover,Shrub_cover,Grass_cover,Forb_cover,Bare_ground,
                     Annuals_binary,C3_cover,C4_cover,
                     Bulk_density,Gravel,Sand,Clay)

#weather database creation for SOILWAT2 input
# Loop through each coordinate in the dataframe
for (i in 1:nrow(NM_point)) {
  #Each of these needs to be a column in your data frame
  lat <- NM_point$Latitude[i]
  lon <- NM_point$Longitude[i]
  Tree_cover<-NM_point$Tree_cover[i]
  Shrub_cover<-NM_point$Shrub_cover[i]
  Forb_cover<-NM_point$Forb_cover[i]
  Grass_cover<-NM_point$Grass_cover[i]
  Bare_ground<-NM_point$Bare_ground[i]
  Annuals_binary<-NM_point$Annuals_binary[i]
  C3_cover<-NM_point$C3_cover[i]
  C4_cover<-NM_point$C4_cover[i] #C3 + C4 need to add up to Grass_cover amount
  Bulk_density<-NM_point$Bulk_density[i]
  Gravel<-NM_point$Gravel[i]
  Sand<-NM_point$Sand[i]
  Clay<-NM_point$Clay[i]
  
  {
    # Download from `DayMet`
    dm_gradient <- try(daymetr::download_daymet(
      site =  paste0("NM_point", i), 
      lat = lat,
      lon = lon,
      start = 1982,
      end = 2024,
      internal = TRUE,
      simplify = FALSE
    ))
    
    if (!inherits(dm_gradient, "try-error")) {
      # Convert data to a `rSOILWAT2`-formatted weather object
      vars <- c("year", "yday", "tmax..deg.c.", "tmin..deg.c.", "prcp..mm.day.")
      xdf <- dm_gradient[["data"]][, vars]
      xdf[, "prcp..mm.day."] <- xdf[, "prcp..mm.day."] / 10 # convert mm -> cm
      colnames(xdf) <- c("Year", "DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      
      xdf$Year <- xdf$Year - (1982 - 1968)
      wdata_dm <- rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf)
      
      # Convert `DayMet`'s `noleap` calendar to proleptic Gregorian calendar
      xdf2 <- rSOILWAT2::dbW_convert_to_GregorianYears(weatherData = wdata_dm)
      
      wdata <- rSOILWAT2::dbW_generateWeather(
        weatherData = rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf2),
        seed = 123
      )
      
      # Check that weather data is well-formed
      stopifnot(rSOILWAT2::dbW_check_weatherData(wdata))
    }
    
    
    clim <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, do_C4vars = TRUE)
    #Generate data off Weekmet data
    veg_cover <- rSOILWAT2::estimate_PotNatVeg_composition(
      MAP_mm = 10 * clim[["MAP_cm"]],
      MAT_C = clim[["MAT_C"]],
      mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
      mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
      dailyC4vars = clim[["dailyC4vars"]]
    )
    
    #Put in your vegetation values below
    
    veg_cover[["Rel_Abundance_L1"]][["SW_TREES"]]<- Tree_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_SHRUB"]]<- Shrub_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_FORBS"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_GRASS"]]<-Grass_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_BAREGROUND"]]<-Bare_ground
    
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]]<-C3_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]]<-C4_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]]<-1 #Binary between 1 and 0
    veg_cover[["Rel_Abundance_L0"]][["Shrubs"]]<-Shrub_cover
    veg_cover[["Rel_Abundance_L0"]][["Forbs"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L0"]][["Trees"]]<-Tree_cover
    veg_cover[["Rel_Abundance_L0"]][["BareGround"]]<-Bare_ground
    veg_cover[["Rel_Abundance_L0"]][["Succulents"]]<-0
    
    ids <- sapply(
      X = names(rSOILWAT2::swProd_Composition(sw_in)),
      FUN = function(x) {
        grep(
          pattern = substr(x, 1, 4),
          x = names(veg_cover[["Rel_Abundance_L1"]]),
          ignore.case = TRUE
        )
      }
    )
    # Assign fractional cover values to rSOILWAT2 input object
    rSOILWAT2::swProd_Composition(sw_in) <- veg_cover[["Rel_Abundance_L1"]][ids]
    ### Biomass amount and phenology (for shrubs and grasses)
    # Reference biomass values from Bradford et al. 2014 are used
    # Mean monthly reference temperature corresponding to default phenology values
    # for the median across 898 big sagebrush sites are used
    
    
    veg_biom <- rSOILWAT2::estimate_PotNatVeg_biomass(
      target_temp = clim[["meanMonthlyTempC"]],
      target_MAP_mm = 10 * clim[["MAP_cm"]],
      do_adjust_phenology = TRUE,
      do_adjust_biomass = TRUE,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Assign monthly biomass values to rSOILWAT2 input object
    # Note: monthly biomass values of forbs, trees, etc. need to be estimated
    v1 <- c("Litter", "Biomass", "Perc.Live")
    v2 <- c("Litter", "Biomass", "Live_pct")
    rSOILWAT2::swProd_MonProd_grass(sw_in)[, v2] <- veg_biom[["grass"]][, v1]
    rSOILWAT2::swProd_MonProd_shrub(sw_in)[, v2] <- veg_biom[["shrub"]][, v1]
    
    ### Rooting profiles of vegetation types
    
    # Select rooting profile types
    # Set those to "FILL" where cover == 0 (because of transpiration regions)
    trco_type_by_veg <- list(
      grass_C3 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_C4 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_annuals = if (
        veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]] > 0
      ) {
        "Jacksonetal1996_crops"
      } else {
        "FILL"
      },
      shrub = if (veg_cover[["Rel_Abundance_L0"]][["Shrubs"]] > 0) {
        "SchenkJackson2003_PCdry_shrubs"
      } else {
        "FILL"
      },
      forb = if (veg_cover[["Rel_Abundance_L0"]][["Forbs"]] > 0) {
        "SchenkJackson2003_PCdry_forbs"
      } else {
        "FILL"
      },
      tree = if (veg_cover[["Rel_Abundance_L0"]][["Trees"]] > 0) {
        "Bradfordetal2014_LodgepolePine"
      } else {
        "FILL"
      }
    )
    
    ### Create shell soil data base:
    soil_fixed <- data.frame(
      depth_cm = c(5, 10, 20, 30, 40,60,80,100),
      bulkDensity_g.cm.3 = rep(Bulk_density, 8),
      gravel_content = rep(Gravel, 8),
      EvapBareSoil_frac = c(0.9182,0.0818,0.000,0.000,0.000,0.000,0.000,0.000),
      transpGrass_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      transpShrub_frac = c(0.1585857,0.1414536,0.2387138,0.4612469,0.4612469,0.2387138,0.4612469,0.4612469),
      transpTree_frac =  c(0.1,0.1,0.2,0.6,0.4,0.2,0.6,0.4),
      transpForb_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      sand_frac = rep(Sand, 8),
      clay_frac = rep(Clay, 8),
      impermeability_frac = c(0,0,0,0,0,0,0,0),
      soilTemp_c = c(-0.9803060,-0.9606121,-0.9212242,-0.8030605, -0.8030605,-0.9212242,-0.8030605, -0.8030605)
    )
    soil_new <- data.frame(rSOILWAT2::swSoils_Layers(sw_in)[0, ])
    
    soil_new[seq_len(nrow(soil_fixed)), ] <- soil_fixed
    
    veg_roots <- rSOILWAT2::estimate_PotNatVeg_roots(
      layers_depth = soil_new[, "depth_cm"],
      trco_type_by_veg = trco_type_by_veg,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Add rooting profile to soil
    v1 <- c("Grass", "Shrub", "Tree", "Forb")
    v2 <- paste0("transp", v1, "_frac")
    soil_new[, v2] <- veg_roots[, v1]
    # Create a new sw_in object with soil_new
    rSOILWAT2::swSoils_Layers(sw_in) <- data.matrix(soil_new)
    # Run sw_in through rSOILWAT2::sw_exec() to create sw_out
    sw_out <- rSOILWAT2::sw_exec(inputData = sw_in, weatherList = wdata)
    
    NM_output_TRANPS <- as.data.frame(sw_out@TRANSP@Day)
  }
}  

transp_NM <- NM_output_TRANPS %>%
  mutate(Year = Year + 13) %>%
  rename(DOY = Day, year = Year) %>%
  dplyr::select(1:10)

transp_NM<-transp_NM%>%
  group_by(year,DOY)%>%
  mutate(total = sum(transp_total_Lyr_1,transp_total_Lyr_2,transp_total_Lyr_3,transp_total_Lyr_4,
                     transp_total_Lyr_5,transp_total_Lyr_6,transp_total_Lyr_7,transp_total_Lyr_8))


write.csv(transp_NM, "~/Desktop/Thesis Stuff/phenology/soilwatdata/transpiration/transp_NM.csv")

#-----------------------SCOLO---------------------------
site<-c("SCOLO")
Latitude<-c(38.200340)
Longitude<-c( -102.470502)
Tree_cover<-c(0)                                                                                                                           
Shrub_cover<-c(.09)
Grass_cover<-c(.78)
Forb_cover<-c(.13)
Bare_ground<-c(0)
Annuals_binary<-c(1)
C3_cover<-c(.2)
C4_cover<-c(.8)
Bulk_density<-c(1.7)
Gravel<-c(0)
Sand<-c(.2)
Clay<-c(.3)

SCOLO_point<-data.frame(site,Latitude,Longitude,Tree_cover,Shrub_cover,Grass_cover,Forb_cover,Bare_ground,
                        Annuals_binary,C3_cover,C4_cover,
                        Bulk_density,Gravel,Sand,Clay)

#weather database creation for SOILWAT2 input
# Loop through each coordinate in the dataframe
for (i in 1:nrow(SCOLO_point)) {
  #Each of these needs to be a column in your data frame
  lat <- SCOLO_point$Latitude[i]
  lon <- SCOLO_point$Longitude[i]
  Tree_cover<-SCOLO_point$Tree_cover[i]
  Shrub_cover<-SCOLO_point$Shrub_cover[i]
  Forb_cover<-SCOLO_point$Forb_cover[i]
  Grass_cover<-SCOLO_point$Grass_cover[i]
  Bare_ground<-SCOLO_point$Bare_ground[i]
  Annuals_binary<-SCOLO_point$Annuals_binary[i]
  C3_cover<-SCOLO_point$C3_cover[i]
  C4_cover<-SCOLO_point$C4_cover[i] #C3 + C4 need to add up to Grass_cover amount
  Bulk_density<-SCOLO_point$Bulk_density[i]
  Gravel<-SCOLO_point$Gravel[i]
  Sand<-SCOLO_point$Sand[i]
  Clay<-SCOLO_point$Clay[i]
  
  {
    # Download from `DayMet`
    dm_gradient <- try(daymetr::download_daymet(
      site =  paste0("SCOLO_point", i), 
      lat = lat,
      lon = lon,
      start = 1982,
      end = 2024,
      internal = TRUE,
      simplify = FALSE
    ))
    
    if (!inherits(dm_gradient, "try-error")) {
      # Convert data to a `rSOILWAT2`-formatted weather object
      vars <- c("year", "yday", "tmax..deg.c.", "tmin..deg.c.", "prcp..mm.day.")
      xdf <- dm_gradient[["data"]][, vars]
      xdf[, "prcp..mm.day."] <- xdf[, "prcp..mm.day."] / 10 # convert mm -> cm
      colnames(xdf) <- c("Year", "DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      
      xdf$Year <- xdf$Year - (1982 - 1968)
      wdata_dm <- rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf)
      
      # Convert `DayMet`'s `noleap` calendar to proleptic Gregorian calendar
      xdf2 <- rSOILWAT2::dbW_convert_to_GregorianYears(weatherData = wdata_dm)
      
      wdata <- rSOILWAT2::dbW_generateWeather(
        weatherData = rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf2),
        seed = 123
      )
      
      # Check that weather data is well-formed
      stopifnot(rSOILWAT2::dbW_check_weatherData(wdata))
    }
    
    
    clim <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, do_C4vars = TRUE)
    #Generate data off Weekmet data
    veg_cover <- rSOILWAT2::estimate_PotNatVeg_composition(
      MAP_mm = 10 * clim[["MAP_cm"]],
      MAT_C = clim[["MAT_C"]],
      mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
      mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
      dailyC4vars = clim[["dailyC4vars"]]
    )
    
    #Put in your vegetation values below
    
    veg_cover[["Rel_Abundance_L1"]][["SW_TREES"]]<- Tree_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_SHRUB"]]<- Shrub_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_FORBS"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_GRASS"]]<-Grass_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_BAREGROUND"]]<-Bare_ground
    
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]]<-C3_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]]<-C4_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]]<-1 #Binary between 1 and 0
    veg_cover[["Rel_Abundance_L0"]][["Shrubs"]]<-Shrub_cover
    veg_cover[["Rel_Abundance_L0"]][["Forbs"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L0"]][["Trees"]]<-Tree_cover
    veg_cover[["Rel_Abundance_L0"]][["BareGround"]]<-Bare_ground
    veg_cover[["Rel_Abundance_L0"]][["Succulents"]]<-0
    
    ids <- sapply(
      X = names(rSOILWAT2::swProd_Composition(sw_in)),
      FUN = function(x) {
        grep(
          pattern = substr(x, 1, 4),
          x = names(veg_cover[["Rel_Abundance_L1"]]),
          ignore.case = TRUE
        )
      }
    )
    # Assign fractional cover values to rSOILWAT2 input object
    rSOILWAT2::swProd_Composition(sw_in) <- veg_cover[["Rel_Abundance_L1"]][ids]
    ### Biomass amount and phenology (for shrubs and grasses)
    # Reference biomass values from Bradford et al. 2014 are used
    # Mean monthly reference temperature corresponding to default phenology values
    # for the median across 898 big sagebrush sites are used
    
    
    veg_biom <- rSOILWAT2::estimate_PotNatVeg_biomass(
      target_temp = clim[["meanMonthlyTempC"]],
      target_MAP_mm = 10 * clim[["MAP_cm"]],
      do_adjust_phenology = TRUE,
      do_adjust_biomass = TRUE,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Assign monthly biomass values to rSOILWAT2 input object
    # Note: monthly biomass values of forbs, trees, etc. need to be estimated
    v1 <- c("Litter", "Biomass", "Perc.Live")
    v2 <- c("Litter", "Biomass", "Live_pct")
    rSOILWAT2::swProd_MonProd_grass(sw_in)[, v2] <- veg_biom[["grass"]][, v1]
    rSOILWAT2::swProd_MonProd_shrub(sw_in)[, v2] <- veg_biom[["shrub"]][, v1]
    
    ### Rooting profiles of vegetation types
    
    # Select rooting profile types
    # Set those to "FILL" where cover == 0 (because of transpiration regions)
    trco_type_by_veg <- list(
      grass_C3 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_C4 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_annuals = if (
        veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]] > 0
      ) {
        "Jacksonetal1996_crops"
      } else {
        "FILL"
      },
      shrub = if (veg_cover[["Rel_Abundance_L0"]][["Shrubs"]] > 0) {
        "SchenkJackson2003_PCdry_shrubs"
      } else {
        "FILL"
      },
      forb = if (veg_cover[["Rel_Abundance_L0"]][["Forbs"]] > 0) {
        "SchenkJackson2003_PCdry_forbs"
      } else {
        "FILL"
      },
      tree = if (veg_cover[["Rel_Abundance_L0"]][["Trees"]] > 0) {
        "Bradfordetal2014_LodgepolePine"
      } else {
        "FILL"
      }
    )
    
    ### Create shell soil data base:
    soil_fixed <- data.frame(
      depth_cm = c(5, 10, 20, 30, 40,60,80,100),
      bulkDensity_g.cm.3 = rep(Bulk_density, 8),
      gravel_content = rep(Gravel, 8),
      EvapBareSoil_frac = c(0.9182,0.0818,0.000,0.000,0.000,0.000,0.000,0.000),
      transpGrass_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      transpShrub_frac = c(0.1585857,0.1414536,0.2387138,0.4612469,0.4612469,0.2387138,0.4612469,0.4612469),
      transpTree_frac =  c(0.1,0.1,0.2,0.6,0.4,0.2,0.6,0.4),
      transpForb_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      sand_frac = rep(Sand, 8),
      clay_frac = rep(Clay, 8),
      impermeability_frac = c(0,0,0,0,0,0,0,0),
      soilTemp_c = c(-0.9803060,-0.9606121,-0.9212242,-0.8030605, -0.8030605,-0.9212242,-0.8030605, -0.8030605)
    )
    soil_new <- data.frame(rSOILWAT2::swSoils_Layers(sw_in)[0, ])
    
    soil_new[seq_len(nrow(soil_fixed)), ] <- soil_fixed
    
    veg_roots <- rSOILWAT2::estimate_PotNatVeg_roots(
      layers_depth = soil_new[, "depth_cm"],
      trco_type_by_veg = trco_type_by_veg,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Add rooting profile to soil
    v1 <- c("Grass", "Shrub", "Tree", "Forb")
    v2 <- paste0("transp", v1, "_frac")
    soil_new[, v2] <- veg_roots[, v1]
    # Create a new sw_in object with soil_new
    rSOILWAT2::swSoils_Layers(sw_in) <- data.matrix(soil_new)
    # Run sw_in through rSOILWAT2::sw_exec() to create sw_out
    sw_out <- rSOILWAT2::sw_exec(inputData = sw_in, weatherList = wdata)
    
    SCOLO_output_TRANSP <- as.data.frame(sw_out@TRANSP@Day)
  }
}  


transp_SCOLO <- SCOLO_output_TRANSP %>%
  mutate(Year = Year + 13) %>%
  rename(DOY = Day, year = Year) %>%
  dplyr::select(1:10)

transp_SCOLO<-transp_SCOLO%>%
  group_by(year,DOY)%>%
  mutate(total = sum(transp_total_Lyr_1,transp_total_Lyr_2,transp_total_Lyr_3,transp_total_Lyr_4,
                     transp_total_Lyr_5,transp_total_Lyr_6,transp_total_Lyr_7,transp_total_Lyr_8))


write.csv(transp_SCOLO, "~/Desktop/Thesis Stuff/phenology/soilwatdata/transpiration/transp_SCOLO.csv")

#----------------------------------------------NCOLO---------------
site<-c("NCOLO")
Latitude<-c(40.835522)
Longitude<-c(-104.761907)
Tree_cover<-c(0)                                                                                                                           
Shrub_cover<-c(.09)
Grass_cover<-c(.78)
Forb_cover<-c(.13)
Bare_ground<-c(0)
Annuals_binary<-c(1)
C3_cover<-c(.2)
C4_cover<-c(.8)
Bulk_density<-c(1.56)
Gravel<-c(0)
Sand<-c(.616)
Clay<-c(.15)

NCOLO_point<-data.frame(site,Latitude,Longitude,Tree_cover,Shrub_cover,Grass_cover,Forb_cover,Bare_ground,
                        Annuals_binary,C3_cover,C4_cover,
                        Bulk_density,Gravel,Sand,Clay)


for (i in 1:nrow(NCOLO_point)) {
  #Each of these needs to be a column in your data frame
  lat <- NCOLO_point$Latitude[i]
  lon <- NCOLO_point$Longitude[i]
  Tree_cover<-NCOLO_point$Tree_cover[i]
  Shrub_cover<-NCOLO_point$Shrub_cover[i]
  Forb_cover<-NCOLO_point$Forb_cover[i]
  Grass_cover<-NCOLO_point$Grass_cover[i]
  Bare_ground<-NCOLO_point$Bare_ground[i]
  Annuals_binary<-NCOLO_point$Annuals_binary[i]
  C3_cover<-NCOLO_point$C3_cover[i]
  C4_cover<-NCOLO_point$C4_cover[i] #C3 + C4 need to add up to Grass_cover amount
  Bulk_density<-NCOLO_point$Bulk_density[i]
  Gravel<-NCOLO_point$Gravel[i]
  Sand<-NCOLO_point$Sand[i]
  Clay<-NCOLO_point$Clay[i]
  
  {
    # Download from `DayMet`
    dm_gradient <- try(daymetr::download_daymet(
      site =  paste0("NCOLO_point", i), 
      lat = lat,
      lon = lon,
      start = 1982,
      end = 2024,
      internal = TRUE,
      simplify = FALSE
    ))
    
    if (!inherits(dm_gradient, "try-error")) {
      # Convert data to a `rSOILWAT2`-formatted weather object
      vars <- c("year", "yday", "tmax..deg.c.", "tmin..deg.c.", "prcp..mm.day.")
      xdf <- dm_gradient[["data"]][, vars]
      xdf[, "prcp..mm.day."] <- xdf[, "prcp..mm.day."] / 10 # convert mm -> cm
      colnames(xdf) <- c("Year", "DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      
      xdf$Year <- xdf$Year - (1982 - 1968)
      wdata_dm <- rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf)
      
      # Convert `DayMet`'s `noleap` calendar to proleptic Gregorian calendar
      xdf2 <- rSOILWAT2::dbW_convert_to_GregorianYears(weatherData = wdata_dm)
      
      wdata <- rSOILWAT2::dbW_generateWeather(
        weatherData = rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf2),
        seed = 123
      )
      
      # Check that weather data is well-formed
      stopifnot(rSOILWAT2::dbW_check_weatherData(wdata))
    }
    
    
    clim <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, do_C4vars = TRUE)
    #Generate data off Weekmet data
    veg_cover <- rSOILWAT2::estimate_PotNatVeg_composition(
      MAP_mm = 10 * clim[["MAP_cm"]],
      MAT_C = clim[["MAT_C"]],
      mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
      mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
      dailyC4vars = clim[["dailyC4vars"]]
    )
    
    #Put in your vegetation values below
    
    veg_cover[["Rel_Abundance_L1"]][["SW_TREES"]]<- Tree_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_SHRUB"]]<- Shrub_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_FORBS"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_GRASS"]]<-Grass_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_BAREGROUND"]]<-Bare_ground
    
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]]<-C3_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]]<-C4_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]]<-1 #Binary between 1 and 0
    veg_cover[["Rel_Abundance_L0"]][["Shrubs"]]<-Shrub_cover
    veg_cover[["Rel_Abundance_L0"]][["Forbs"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L0"]][["Trees"]]<-Tree_cover
    veg_cover[["Rel_Abundance_L0"]][["BareGround"]]<-Bare_ground
    veg_cover[["Rel_Abundance_L0"]][["Succulents"]]<-0
    
    ids <- sapply(
      X = names(rSOILWAT2::swProd_Composition(sw_in)),
      FUN = function(x) {
        grep(
          pattern = substr(x, 1, 4),
          x = names(veg_cover[["Rel_Abundance_L1"]]),
          ignore.case = TRUE
        )
      }
    )
    # Assign fractional cover values to rSOILWAT2 input object
    rSOILWAT2::swProd_Composition(sw_in) <- veg_cover[["Rel_Abundance_L1"]][ids]
    ### Biomass amount and phenology (for shrubs and grasses)
    # Reference biomass values from Bradford et al. 2014 are used
    # Mean monthly reference temperature corresponding to default phenology values
    # for the median across 898 big sagebrush sites are used
    
    
    veg_biom <- rSOILWAT2::estimate_PotNatVeg_biomass(
      target_temp = clim[["meanMonthlyTempC"]],
      target_MAP_mm = 10 * clim[["MAP_cm"]],
      do_adjust_phenology = TRUE,
      do_adjust_biomass = TRUE,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Assign monthly biomass values to rSOILWAT2 input object
    # Note: monthly biomass values of forbs, trees, etc. need to be estimated
    v1 <- c("Litter", "Biomass", "Perc.Live")
    v2 <- c("Litter", "Biomass", "Live_pct")
    rSOILWAT2::swProd_MonProd_grass(sw_in)[, v2] <- veg_biom[["grass"]][, v1]
    rSOILWAT2::swProd_MonProd_shrub(sw_in)[, v2] <- veg_biom[["shrub"]][, v1]
    
    ### Rooting profiles of vegetation types
    
    # Select rooting profile types
    # Set those to "FILL" where cover == 0 (because of transpiration regions)
    trco_type_by_veg <- list(
      grass_C3 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_C4 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_annuals = if (
        veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]] > 0
      ) {
        "Jacksonetal1996_crops"
      } else {
        "FILL"
      },
      shrub = if (veg_cover[["Rel_Abundance_L0"]][["Shrubs"]] > 0) {
        "SchenkJackson2003_PCdry_shrubs"
      } else {
        "FILL"
      },
      forb = if (veg_cover[["Rel_Abundance_L0"]][["Forbs"]] > 0) {
        "SchenkJackson2003_PCdry_forbs"
      } else {
        "FILL"
      },
      tree = if (veg_cover[["Rel_Abundance_L0"]][["Trees"]] > 0) {
        "Bradfordetal2014_LodgepolePine"
      } else {
        "FILL"
      }
    )
    
    ### Create shell soil data base:
    soil_fixed <- data.frame(
      depth_cm = c(5, 10, 20, 30, 40,60,80,100),
      bulkDensity_g.cm.3 = rep(Bulk_density, 8),
      gravel_content = rep(Gravel, 8),
      EvapBareSoil_frac = c(0.9182,0.0818,0.000,0.000,0.000,0.000,0.000,0.000),
      transpGrass_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      transpShrub_frac = c(0.1585857,0.1414536,0.2387138,0.4612469,0.4612469,0.2387138,0.4612469,0.4612469),
      transpTree_frac =  c(0.1,0.1,0.2,0.6,0.4,0.2,0.6,0.4),
      transpForb_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      sand_frac = rep(Sand, 8),
      clay_frac = rep(Clay, 8),
      impermeability_frac = c(0,0,0,0,0,0,0,0),
      soilTemp_c = c(-0.9803060,-0.9606121,-0.9212242,-0.8030605, -0.8030605,-0.9212242,-0.8030605, -0.8030605)
    )
    soil_new <- data.frame(rSOILWAT2::swSoils_Layers(sw_in)[0, ])
    
    soil_new[seq_len(nrow(soil_fixed)), ] <- soil_fixed
    
    veg_roots <- rSOILWAT2::estimate_PotNatVeg_roots(
      layers_depth = soil_new[, "depth_cm"],
      trco_type_by_veg = trco_type_by_veg,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Add rooting profile to soil
    v1 <- c("Grass", "Shrub", "Tree", "Forb")
    v2 <- paste0("transp", v1, "_frac")
    soil_new[, v2] <- veg_roots[, v1]
    # Create a new sw_in object with soil_new
    rSOILWAT2::swSoils_Layers(sw_in) <- data.matrix(soil_new)
    # Run sw_in through rSOILWAT2::sw_exec() to create sw_out
    sw_out <- rSOILWAT2::sw_exec(inputData = sw_in, weatherList = wdata)
    
    NCOLO_output_TRANSP <- as.data.frame(sw_out@TRANSP@Day)
  }
}  

transp_NCOLO <- NCOLO_output_TRANSP %>%
  mutate(Year = Year + 13) %>%
  rename(DOY = Day, year = Year) %>%
  dplyr::select(1:10)

transp_NCOLO<-transp_NCOLO%>%
  group_by(year,DOY)%>%
  mutate(total = sum(transp_total_Lyr_1,transp_total_Lyr_2,transp_total_Lyr_3,transp_total_Lyr_4,
                     transp_total_Lyr_5,transp_total_Lyr_6,transp_total_Lyr_7,transp_total_Lyr_8))


write.csv(transp_NCOLO, "~/Desktop/Thesis Stuff/phenology/soilwatdata/transpiration/transp_NCOLO.csv")

#-------------------WY------------------------------------
#WY
site<-c("WY")
Latitude<-c(43.34193)
Longitude<-c(-105.1387267)
Tree_cover<-c(0)                                                                                                                           
Shrub_cover<-c(.1)
Grass_cover<-c(.75)
Forb_cover<-c(.15)
Bare_ground<-c(.3)
Annuals_binary<-c(1)
C3_cover<-c(.4)
C4_cover<-c(.6)
Bulk_density<-c(1.50)
Gravel<-c(0)
Sand<-c(.561)
Clay<-c(.267)

WY_point<-data.frame(site,Latitude,Longitude,Tree_cover,Shrub_cover,Grass_cover,Forb_cover,Bare_ground,
                     Annuals_binary,C3_cover,C4_cover,
                     Bulk_density,Gravel,Sand,Clay)


#weather database creation for SOILWAT2 input
for (i in 1:nrow(WY_point)) {
  #Each of these needs to be a column in your data frame
  lat <- WY_point$Latitude[i]
  lon <- WY_point$Longitude[i]
  Tree_cover<-WY_point$Tree_cover[i]
  Shrub_cover<-WY_point$Shrub_cover[i]
  Forb_cover<-WY_point$Forb_cover[i]
  Grass_cover<-WY_point$Grass_cover[i]
  Bare_ground<-WY_point$Bare_ground[i]
  Annuals_binary<-WY_point$Annuals_binary[i]
  C3_cover<-WY_point$C3_cover[i]
  C4_cover<-WY_point$C4_cover[i] #C3 + C4 need to add up to Grass_cover amount
  Bulk_density<-WY_point$Bulk_density[i]
  Gravel<-WY_point$Gravel[i]
  Sand<-WY_point$Sand[i]
  Clay<-WY_point$Clay[i]
  
  {
    # Download from `DayMet`
    dm_gradient <- try(daymetr::download_daymet(
      site =  paste0("WY_point", i), 
      lat = lat,
      lon = lon,
      start = 1982,
      end = 2024,
      internal = TRUE,
      simplify = FALSE
    ))
    
    if (!inherits(dm_gradient, "try-error")) {
      # Convert data to a `rSOILWAT2`-formatted weather object
      vars <- c("year", "yday", "tmax..deg.c.", "tmin..deg.c.", "prcp..mm.day.")
      xdf <- dm_gradient[["data"]][, vars]
      xdf[, "prcp..mm.day."] <- xdf[, "prcp..mm.day."] / 10 # convert mm -> cm
      colnames(xdf) <- c("Year", "DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      
      xdf$Year <- xdf$Year - (1982 - 1968)
      wdata_dm <- rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf)
      
      # Convert `DayMet`'s `noleap` calendar to proleptic Gregorian calendar
      xdf2 <- rSOILWAT2::dbW_convert_to_GregorianYears(weatherData = wdata_dm)
      
      wdata <- rSOILWAT2::dbW_generateWeather(
        weatherData = rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf2),
        seed = 123
      )
      
      # Check that weather data is well-formed
      stopifnot(rSOILWAT2::dbW_check_weatherData(wdata))
    }
    
    
    clim <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, do_C4vars = TRUE)
    #Generate data off Weekmet data
    veg_cover <- rSOILWAT2::estimate_PotNatVeg_composition(
      MAP_mm = 10 * clim[["MAP_cm"]],
      MAT_C = clim[["MAT_C"]],
      mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
      mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
      dailyC4vars = clim[["dailyC4vars"]]
    )
    
    #Put in your vegetation values below
    
    veg_cover[["Rel_Abundance_L1"]][["SW_TREES"]]<- Tree_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_SHRUB"]]<- Shrub_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_FORBS"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_GRASS"]]<-Grass_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_BAREGROUND"]]<-Bare_ground
    
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]]<-C3_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]]<-C4_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]]<-1 #Binary between 1 and 0
    veg_cover[["Rel_Abundance_L0"]][["Shrubs"]]<-Shrub_cover
    veg_cover[["Rel_Abundance_L0"]][["Forbs"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L0"]][["Trees"]]<-Tree_cover
    veg_cover[["Rel_Abundance_L0"]][["BareGround"]]<-Bare_ground
    veg_cover[["Rel_Abundance_L0"]][["Succulents"]]<-0
    
    ids <- sapply(
      X = names(rSOILWAT2::swProd_Composition(sw_in)),
      FUN = function(x) {
        grep(
          pattern = substr(x, 1, 4),
          x = names(veg_cover[["Rel_Abundance_L1"]]),
          ignore.case = TRUE
        )
      }
    )
    # Assign fractional cover values to rSOILWAT2 input object
    rSOILWAT2::swProd_Composition(sw_in) <- veg_cover[["Rel_Abundance_L1"]][ids]
    ### Biomass amount and phenology (for shrubs and grasses)
    # Reference biomass values from Bradford et al. 2014 are used
    # Mean monthly reference temperature corresponding to default phenology values
    # for the median across 898 big sagebrush sites are used
    
    
    veg_biom <- rSOILWAT2::estimate_PotNatVeg_biomass(
      target_temp = clim[["meanMonthlyTempC"]],
      target_MAP_mm = 10 * clim[["MAP_cm"]],
      do_adjust_phenology = TRUE,
      do_adjust_biomass = TRUE,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Assign monthly biomass values to rSOILWAT2 input object
    # Note: monthly biomass values of forbs, trees, etc. need to be estimated
    v1 <- c("Litter", "Biomass", "Perc.Live")
    v2 <- c("Litter", "Biomass", "Live_pct")
    rSOILWAT2::swProd_MonProd_grass(sw_in)[, v2] <- veg_biom[["grass"]][, v1]
    rSOILWAT2::swProd_MonProd_shrub(sw_in)[, v2] <- veg_biom[["shrub"]][, v1]
    
    ### Rooting profiles of vegetation types
    
    # Select rooting profile types
    # Set those to "FILL" where cover == 0 (because of transpiration regions)
    trco_type_by_veg <- list(
      grass_C3 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_C4 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_annuals = if (
        veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]] > 0
      ) {
        "Jacksonetal1996_crops"
      } else {
        "FILL"
      },
      shrub = if (veg_cover[["Rel_Abundance_L0"]][["Shrubs"]] > 0) {
        "SchenkJackson2003_PCdry_shrubs"
      } else {
        "FILL"
      },
      forb = if (veg_cover[["Rel_Abundance_L0"]][["Forbs"]] > 0) {
        "SchenkJackson2003_PCdry_forbs"
      } else {
        "FILL"
      },
      tree = if (veg_cover[["Rel_Abundance_L0"]][["Trees"]] > 0) {
        "Bradfordetal2014_LodgepolePine"
      } else {
        "FILL"
      }
    )
    
    ### Create shell soil data base:
    soil_fixed <- data.frame(
      depth_cm = c(5, 10, 20, 30, 40,60,80,100),
      bulkDensity_g.cm.3 = rep(Bulk_density, 8),
      gravel_content = rep(Gravel, 8),
      EvapBareSoil_frac = c(0.9182,0.0818,0.000,0.000,0.000,0.000,0.000,0.000),
      transpGrass_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      transpShrub_frac = c(0.1585857,0.1414536,0.2387138,0.4612469,0.4612469,0.2387138,0.4612469,0.4612469),
      transpTree_frac =  c(0.1,0.1,0.2,0.6,0.4,0.2,0.6,0.4),
      transpForb_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      sand_frac = rep(Sand, 8),
      clay_frac = rep(Clay, 8),
      impermeability_frac = c(0,0,0,0,0,0,0,0),
      soilTemp_c = c(-0.9803060,-0.9606121,-0.9212242,-0.8030605, -0.8030605,-0.9212242,-0.8030605, -0.8030605)
    )
    soil_new <- data.frame(rSOILWAT2::swSoils_Layers(sw_in)[0, ])
    
    soil_new[seq_len(nrow(soil_fixed)), ] <- soil_fixed
    
    veg_roots <- rSOILWAT2::estimate_PotNatVeg_roots(
      layers_depth = soil_new[, "depth_cm"],
      trco_type_by_veg = trco_type_by_veg,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Add rooting profile to soil
    v1 <- c("Grass", "Shrub", "Tree", "Forb")
    v2 <- paste0("transp", v1, "_frac")
    soil_new[, v2] <- veg_roots[, v1]
    # Create a new sw_in object with soil_new
    rSOILWAT2::swSoils_Layers(sw_in) <- data.matrix(soil_new)
    # Run sw_in through rSOILWAT2::sw_exec() to create sw_out
    sw_out <- rSOILWAT2::sw_exec(inputData = sw_in, weatherList = wdata)
    
    WY_output_TRANSP <- as.data.frame(sw_out@TRANSP@Day)
  }  
}
transp_WY <- WY_output_TRANSP %>%
  mutate(Year = Year + 13) %>%
  rename(DOY = Day, year = Year) %>%
  dplyr::select(1:10)

transp_WY<-transp_WY%>%
  group_by(year,DOY)%>%
  mutate(total = sum(transp_total_Lyr_1,transp_total_Lyr_2,transp_total_Lyr_3,transp_total_Lyr_4,
                     transp_total_Lyr_5,transp_total_Lyr_6,transp_total_Lyr_7,transp_total_Lyr_8))


write.csv(transp_WY, "~/Desktop/Thesis Stuff/phenology/soilwatdata/transpiration/transp_WY.csv")

#-------------------------------------------MT------------------------------------
site<-c("MT")
Latitude<-c(47.740648)
Longitude<-c(-107.7699083)
Tree_cover<-c(0)                                                                                                                           
Shrub_cover<-c(.10)
Grass_cover<-c(.85)
Forb_cover<-c(.05)
Bare_ground<-c(.3)
Annuals_binary<-c(1)
C3_cover<-c(.7)
C4_cover<-c(.2)
Bulk_density<-c(1.44)
Gravel<-c(0)
Sand<-c(0.31)
Clay<-c(.375)

MT_point<-data.frame(site,Latitude,Longitude,Tree_cover,Shrub_cover,Grass_cover,Forb_cover,Bare_ground,
                     Annuals_binary,C3_cover,C4_cover,
                     Bulk_density,Gravel,Sand,Clay)


for (i in 1:nrow(MT_point)) {
  #Each of these needs to be a column in your data frame
  lat <- MT_point$Latitude[i]
  lon <- MT_point$Longitude[i]
  Tree_cover<-MT_point$Tree_cover[i]
  Shrub_cover<-MT_point$Shrub_cover[i]
  Forb_cover<-MT_point$Forb_cover[i]
  Grass_cover<-MT_point$Grass_cover[i]
  Bare_ground<-MT_point$Bare_ground[i]
  Annuals_binary<-MT_point$Annuals_binary[i]
  C3_cover<-MT_point$C3_cover[i]
  C4_cover<-MT_point$C4_cover[i] #C3 + C4 need to add up to Grass_cover amount
  Bulk_density<-MT_point$Bulk_density[i]
  Gravel<-MT_point$Gravel[i]
  Sand<-MT_point$Sand[i]
  Clay<-MT_point$Clay[i]
  
  {
    # Download from `DayMet`
    dm_gradient <- try(daymetr::download_daymet(
      site =  paste0("MT_point", i), 
      lat = lat,
      lon = lon,
      start = 1982,
      end = 2024,
      internal = TRUE,
      simplify = FALSE
    ))
    
    if (!inherits(dm_gradient, "try-error")) {
      # Convert data to a `rSOILWAT2`-formatted weather object
      vars <- c("year", "yday", "tmax..deg.c.", "tmin..deg.c.", "prcp..mm.day.")
      xdf <- dm_gradient[["data"]][, vars]
      xdf[, "prcp..mm.day."] <- xdf[, "prcp..mm.day."] / 10 # convert mm -> cm
      colnames(xdf) <- c("Year", "DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      
      xdf$Year <- xdf$Year - (1982-1968)
      wdata_dm <- rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf)
      
      # Convert `DayMet`'s `noleap` calendar to proleptic Gregorian calendar
      xdf2 <- rSOILWAT2::dbW_convert_to_GregorianYears(weatherData = wdata_dm)
      
      wdata <- rSOILWAT2::dbW_generateWeather(
        weatherData = rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf2),
        seed = 123
      )
      
      # Check that weather data is well-formed
      stopifnot(rSOILWAT2::dbW_check_weatherData(wdata))
    }
    
    
    clim <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, do_C4vars = TRUE)
    #Generate data off Weekmet data
    veg_cover <- rSOILWAT2::estimate_PotNatVeg_composition(
      MAP_mm = 10 * clim[["MAP_cm"]],
      MAT_C = clim[["MAT_C"]],
      mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
      mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
      dailyC4vars = clim[["dailyC4vars"]]
    )
    
    #Put in your vegetation values below
    
    veg_cover[["Rel_Abundance_L1"]][["SW_TREES"]]<- Tree_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_SHRUB"]]<- Shrub_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_FORBS"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_GRASS"]]<-Grass_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_BAREGROUND"]]<-Bare_ground
    
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]]<-C3_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]]<-C4_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]]<-1 #Binary between 1 and 0
    veg_cover[["Rel_Abundance_L0"]][["Shrubs"]]<-Shrub_cover
    veg_cover[["Rel_Abundance_L0"]][["Forbs"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L0"]][["Trees"]]<-Tree_cover
    veg_cover[["Rel_Abundance_L0"]][["BareGround"]]<-Bare_ground
    veg_cover[["Rel_Abundance_L0"]][["Succulents"]]<-0
    
    ids <- sapply(
      X = names(rSOILWAT2::swProd_Composition(sw_in)),
      FUN = function(x) {
        grep(
          pattern = substr(x, 1, 4),
          x = names(veg_cover[["Rel_Abundance_L1"]]),
          ignore.case = TRUE
        )
      }
    )
    # Assign fractional cover values to rSOILWAT2 input object
    rSOILWAT2::swProd_Composition(sw_in) <- veg_cover[["Rel_Abundance_L1"]][ids]
    ### Biomass amount and phenology (for shrubs and grasses)
    # Reference biomass values from Bradford et al. 2014 are used
    # Mean monthly reference temperature corresponding to default phenology values
    # for the median across 898 big sagebrush sites are used
    
    
    veg_biom <- rSOILWAT2::estimate_PotNatVeg_biomass(
      target_temp = clim[["meanMonthlyTempC"]],
      target_MAP_mm = 10 * clim[["MAP_cm"]],
      do_adjust_phenology = TRUE,
      do_adjust_biomass = TRUE,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Assign monthly biomass values to rSOILWAT2 input object
    # Note: monthly biomass values of forbs, trees, etc. need to be estimated
    v1 <- c("Litter", "Biomass", "Perc.Live")
    v2 <- c("Litter", "Biomass", "Live_pct")
    rSOILWAT2::swProd_MonProd_grass(sw_in)[, v2] <- veg_biom[["grass"]][, v1]
    rSOILWAT2::swProd_MonProd_shrub(sw_in)[, v2] <- veg_biom[["shrub"]][, v1]
    
    ### Rooting profiles of vegetation types
    
    # Select rooting profile types
    # Set those to "FILL" where cover == 0 (because of transpiration regions)
    trco_type_by_veg <- list(
      grass_C3 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_C4 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_annuals = if (
        veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]] > 0
      ) {
        "Jacksonetal1996_crops"
      } else {
        "FILL"
      },
      shrub = if (veg_cover[["Rel_Abundance_L0"]][["Shrubs"]] > 0) {
        "SchenkJackson2003_PCdry_shrubs"
      } else {
        "FILL"
      },
      forb = if (veg_cover[["Rel_Abundance_L0"]][["Forbs"]] > 0) {
        "SchenkJackson2003_PCdry_forbs"
      } else {
        "FILL"
      },
      tree = if (veg_cover[["Rel_Abundance_L0"]][["Trees"]] > 0) {
        "Bradfordetal2014_LodgepolePine"
      } else {
        "FILL"
      }
    )
    
    ### Create shell soil data base:
    soil_fixed <- data.frame(
      depth_cm = c(5, 10, 20, 30, 40,60,80,100),
      bulkDensity_g.cm.3 = rep(Bulk_density, 8),
      gravel_content = rep(Gravel, 8),
      EvapBareSoil_frac = c(0.9182,0.0818,0.000,0.000,0.000,0.000,0.000,0.000),
      transpGrass_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      transpShrub_frac = c(0.1585857,0.1414536,0.2387138,0.4612469,0.4612469,0.2387138,0.4612469,0.4612469),
      transpTree_frac =  c(0.1,0.1,0.2,0.6,0.4,0.2,0.6,0.4),
      transpForb_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      sand_frac = rep(Sand, 8),
      clay_frac = rep(Clay, 8),
      impermeability_frac = c(0,0,0,0,0,0,0,0),
      soilTemp_c = c(-0.9803060,-0.9606121,-0.9212242,-0.8030605, -0.8030605,-0.9212242,-0.8030605, -0.8030605)
    )
    soil_new <- data.frame(rSOILWAT2::swSoils_Layers(sw_in)[0, ])
    
    soil_new[seq_len(nrow(soil_fixed)), ] <- soil_fixed
    
    veg_roots <- rSOILWAT2::estimate_PotNatVeg_roots(
      layers_depth = soil_new[, "depth_cm"],
      trco_type_by_veg = trco_type_by_veg,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Add rooting profile to soil
    v1 <- c("Grass", "Shrub", "Tree", "Forb")
    v2 <- paste0("transp", v1, "_frac")
    soil_new[, v2] <- veg_roots[, v1]
    # Create a new sw_in object with soil_new
    rSOILWAT2::swSoils_Layers(sw_in) <- data.matrix(soil_new)
    # Run sw_in through rSOILWAT2::sw_exec() to create sw_out
    sw_out <- rSOILWAT2::sw_exec(inputData = sw_in, weatherList = wdata)
    
    MT_output_TRANSP <- as.data.frame(sw_out@TRANSP@Day)
  }
}  

transp_MT <- MT_output_TRANSP %>%
  mutate(Year = Year + 13) %>%
  rename(DOY = Day, year = Year) %>%
  dplyr::select(1:10)

transp_MT<-transp_MT%>%
  group_by(year,DOY)%>%
  mutate(total = sum(transp_total_Lyr_1,transp_total_Lyr_2,transp_total_Lyr_3,transp_total_Lyr_4,
                     transp_total_Lyr_5,transp_total_Lyr_6,transp_total_Lyr_7,transp_total_Lyr_8))


write.csv(transp_MT, "~/Desktop/Thesis Stuff/phenology/soilwatdata/transpiration/transp_MT.csv")

#------------------------------------------------------SASK------------------------------------
site<-c("SASK")
Latitude<-c(50.70007)
Longitude<-c(-107.7318717)
Tree_cover<-c(0)                                                                                                                           
Shrub_cover<-c(.10)
Grass_cover<-c(.85)
Forb_cover<-c(.05)
Bare_ground<-c(.3)
Annuals_binary<-c(1)
C3_cover<-c(.9)
C4_cover<-c(.1)
Bulk_density<-c(1.44)
Gravel<-c(0)
Sand<-c(0.31)
Clay<-c(.375)

SASK_point<-data.frame(site,Latitude,Longitude,Tree_cover,Shrub_cover,Grass_cover,Forb_cover,Bare_ground,
                       Annuals_binary,C3_cover,C4_cover,
                       Bulk_density,Gravel,Sand,Clay)


for (i in 1:nrow(SASK_point)) {
  #Each of these needs to be a column in your data frame
  lat <- SASK_point$Latitude[i]
  lon <- SASK_point$Longitude[i]
  Tree_cover<-SASK_point$Tree_cover[i]
  Shrub_cover<-SASK_point$Shrub_cover[i]
  Forb_cover<-SASK_point$Forb_cover[i]
  Grass_cover<-SASK_point$Grass_cover[i]
  Bare_ground<-SASK_point$Bare_ground[i]
  Annuals_binary<-SASK_point$Annuals_binary[i]
  C3_cover<-SASK_point$C3_cover[i]
  C4_cover<-SASK_point$C4_cover[i] #C3 + C4 need to add up to Grass_cover amount
  Bulk_density<-SASK_point$Bulk_density[i]
  Gravel<-SASK_point$Gravel[i]
  Sand<-SASK_point$Sand[i]
  Clay<-SASK_point$Clay[i]
  
  {
    # Download from `DayMet`
    dm_gradient <- try(daymetr::download_daymet(
      site =  paste0("SASK_point", i), 
      lat = lat,
      lon = lon,
      start = 1982,
      end = 2024,
      internal = TRUE,
      simplify = FALSE
    ))
    
    if (!inherits(dm_gradient, "try-error")) {
      # Convert data to a `rSOILWAT2`-formatted weather object
      vars <- c("year", "yday", "tmax..deg.c.", "tmin..deg.c.", "prcp..mm.day.")
      xdf <- dm_gradient[["data"]][, vars]
      xdf[, "prcp..mm.day."] <- xdf[, "prcp..mm.day."] / 10 # convert mm -> cm
      colnames(xdf) <- c("Year", "DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      
      xdf$Year <- xdf$Year - (1982 - 1968)
      wdata_dm <- rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf)
      
      # Convert `DayMet`'s `noleap` calendar to proleptic Gregorian calendar
      xdf2 <- rSOILWAT2::dbW_convert_to_GregorianYears(weatherData = wdata_dm)
      
      wdata <- rSOILWAT2::dbW_generateWeather(
        weatherData = rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf2),
        seed = 123
      )
      
      # Check that weather data is well-formed
      stopifnot(rSOILWAT2::dbW_check_weatherData(wdata))
    }
    
    
    clim <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, do_C4vars = TRUE)
    #Generate data off Weekmet data
    veg_cover <- rSOILWAT2::estimate_PotNatVeg_composition(
      MAP_mm = 10 * clim[["MAP_cm"]],
      MAT_C = clim[["MAT_C"]],
      mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
      mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
      dailyC4vars = clim[["dailyC4vars"]]
    )
    
    #Put in your vegetation values below
    
    veg_cover[["Rel_Abundance_L1"]][["SW_TREES"]]<- Tree_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_SHRUB"]]<- Shrub_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_FORBS"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_GRASS"]]<-Grass_cover
    veg_cover[["Rel_Abundance_L1"]][["SW_BAREGROUND"]]<-Bare_ground
    
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]]<-C3_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]]<-C4_cover
    veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]]<-1 #Binary between 1 and 0
    veg_cover[["Rel_Abundance_L0"]][["Shrubs"]]<-Shrub_cover
    veg_cover[["Rel_Abundance_L0"]][["Forbs"]]<-Forb_cover
    veg_cover[["Rel_Abundance_L0"]][["Trees"]]<-Tree_cover
    veg_cover[["Rel_Abundance_L0"]][["BareGround"]]<-Bare_ground
    veg_cover[["Rel_Abundance_L0"]][["Succulents"]]<-0
    
    ids <- sapply(
      X = names(rSOILWAT2::swProd_Composition(sw_in)),
      FUN = function(x) {
        grep(
          pattern = substr(x, 1, 4),
          x = names(veg_cover[["Rel_Abundance_L1"]]),
          ignore.case = TRUE
        )
      }
    )
    # Assign fractional cover values to rSOILWAT2 input object
    rSOILWAT2::swProd_Composition(sw_in) <- veg_cover[["Rel_Abundance_L1"]][ids]
    ### Biomass amount and phenology (for shrubs and grasses)
    # Reference biomass values from Bradford et al. 2014 are used
    # Mean monthly reference temperature corresponding to default phenology values
    # for the median across 898 big sagebrush sites are used
    
    
    veg_biom <- rSOILWAT2::estimate_PotNatVeg_biomass(
      target_temp = clim[["meanMonthlyTempC"]],
      target_MAP_mm = 10 * clim[["MAP_cm"]],
      do_adjust_phenology = TRUE,
      do_adjust_biomass = TRUE,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Assign monthly biomass values to rSOILWAT2 input object
    # Note: monthly biomass values of forbs, trees, etc. need to be estimated
    v1 <- c("Litter", "Biomass", "Perc.Live")
    v2 <- c("Litter", "Biomass", "Live_pct")
    rSOILWAT2::swProd_MonProd_grass(sw_in)[, v2] <- veg_biom[["grass"]][, v1]
    rSOILWAT2::swProd_MonProd_shrub(sw_in)[, v2] <- veg_biom[["shrub"]][, v1]
    
    ### Rooting profiles of vegetation types
    
    # Select rooting profile types
    # Set those to "FILL" where cover == 0 (because of transpiration regions)
    trco_type_by_veg <- list(
      grass_C3 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_C4 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]] > 0) {
        "SchenkJackson2003_PCdry_grasses"
      } else {
        "FILL"
      },
      grass_annuals = if (
        veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]] > 0
      ) {
        "Jacksonetal1996_crops"
      } else {
        "FILL"
      },
      shrub = if (veg_cover[["Rel_Abundance_L0"]][["Shrubs"]] > 0) {
        "SchenkJackson2003_PCdry_shrubs"
      } else {
        "FILL"
      },
      forb = if (veg_cover[["Rel_Abundance_L0"]][["Forbs"]] > 0) {
        "SchenkJackson2003_PCdry_forbs"
      } else {
        "FILL"
      },
      tree = if (veg_cover[["Rel_Abundance_L0"]][["Trees"]] > 0) {
        "Bradfordetal2014_LodgepolePine"
      } else {
        "FILL"
      }
    )
    
    ### Create shell soil data base:
    soil_fixed <- data.frame(
      depth_cm = c(5, 10, 20, 30, 40,60,80,100),
      bulkDensity_g.cm.3 = rep(Bulk_density, 8),
      gravel_content = rep(Gravel, 8),
      EvapBareSoil_frac = c(0.9182,0.0818,0.000,0.000,0.000,0.000,0.000,0.000),
      transpGrass_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      transpShrub_frac = c(0.1585857,0.1414536,0.2387138,0.4612469,0.4612469,0.2387138,0.4612469,0.4612469),
      transpTree_frac =  c(0.1,0.1,0.2,0.6,0.4,0.2,0.6,0.4),
      transpForb_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
      sand_frac = rep(Sand, 8),
      clay_frac = rep(Clay, 8),
      impermeability_frac = c(0,0,0,0,0,0,0,0),
      soilTemp_c = c(-0.9803060,-0.9606121,-0.9212242,-0.8030605, -0.8030605,-0.9212242,-0.8030605, -0.8030605)
    )
    soil_new <- data.frame(rSOILWAT2::swSoils_Layers(sw_in)[0, ])
    
    soil_new[seq_len(nrow(soil_fixed)), ] <- soil_fixed
    
    veg_roots <- rSOILWAT2::estimate_PotNatVeg_roots(
      layers_depth = soil_new[, "depth_cm"],
      trco_type_by_veg = trco_type_by_veg,
      fgrass_c3c4ann = veg_cover[["Grasses"]],
    )
    
    # Add rooting profile to soil
    v1 <- c("Grass", "Shrub", "Tree", "Forb")
    v2 <- paste0("transp", v1, "_frac")
    soil_new[, v2] <- veg_roots[, v1]
    # Create a new sw_in object with soil_new
    rSOILWAT2::swSoils_Layers(sw_in) <- data.matrix(soil_new)
    # Run sw_in through rSOILWAT2::sw_exec() to create sw_out
    sw_out <- rSOILWAT2::sw_exec(inputData = sw_in, weatherList = wdata)
    
    SASK_output_TRANSP<- as.data.frame(sw_out@TRANSP@Day)
  }
}  


transp_SASK <- SASK_output_TRANSP %>%
  mutate(Year = Year + 13) %>%
  rename(DOY = Day, year = Year) %>%
  dplyr::select(1:10)

transp_SASK<-transp_SASK%>%
  group_by(year,DOY)%>%
  mutate(total = sum(transp_total_Lyr_1,transp_total_Lyr_2,transp_total_Lyr_3,transp_total_Lyr_4,
                     transp_total_Lyr_5,transp_total_Lyr_6,transp_total_Lyr_7,transp_total_Lyr_8))


write.csv(transp_SASK, "~/Desktop/Thesis Stuff/phenology/soilwatdata/transpiration/transp_SASK.csv")

rm(clim,dm_gradient,MT_point,NCOLO_output_SWP,NCOLO_output_SWP2, NCOLO_point,NM_point,NM_output_SWP,NM_output_SWP2,
   SASK_output_SWP,SASK_output_SWP2,SASK_point,SCOLO_output_SWP,, SCOLO_point,TX_point,TX_output_SWP,
   , WY_point, WY_output_SWP,WY_output_SWP2)

rm(MT_output_SWP,MT_output_SWP2,soil_fixed,soil_new,sw_in,sw_out)
rm(trco_type_by_veg)
rm(veg_biom,veg_cover,veg_roots,wdata,wdata_dm,xdf,xdf2)
