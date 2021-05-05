## 2. Get PRISM Climate Data

# Setup -------------------------------------------------------------------

  options(scipen = 999, stringsAsFactors = F)
  
  library(xlsx); library(climateR); library(AOI); library(raster); library(sf)
  
  datapath <- "D:/Projects/D2P 2021/Data"
  
  # Data Retrieval Dates
  startDate <- "2000-01-01"
  endDate <- "2021-03-10"

# Handle Site Locations ---------------------------------------------------

  sitesData <- readr::read_csv(file.path(datapath, "USGS CO Phys Sites.csv"))
  
  site_sf <- sf::st_as_sf(sitesData,
                          coords = c("dec_long_va", "dec_lat_va"),
                          crs = 4326
  )

# Get Data ----------------------------------------------------------------
  
  climateData <- climateR::getPRISM(site_sf, param = c("prcp", "tmax"), startDate = startDate, endDate = endDate)
  
  
  siteClimate <- climateR::extract_sites(climateData, site_sf, "site_no")
  # siteClimate_raw <- siteClimate # to hold original data while I figure out the rest
  
  names(siteClimate$prcp)[-1] <- paste0(names(siteClimate$prcp)[-1], "_prcp")
  names(siteClimate$tmax)[-1] <- paste0(names(siteClimate$tmax)[-1], "_tmax")
  
  # Combine list elements
  siteClimate <- dplyr::inner_join(
    siteClimate$prcp,
    siteClimate$tmax,
    by = "date"
  )
  
# Write out ---------------------------------------------------------------

  readr::write_csv(siteClimate, file.path(datapath, "PRISM Climate Data.csv"))
  raster::writeRaster(climateData$prcp, file.path(datapath, "Spatial Data/CO Precip.grd"), overwrite = T) 
  raster::writeRaster(climateData$tmax, file.path(datapath, "Spatial Data/CO Temp.grd"), overwrite = T) 
  
  