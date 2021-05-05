## 3. Get NICS Wildfire Data

# Setup -------------------------------------------------------------------

  options(scipen = 999, stringsAsFactors = F)
  
  library(sf); library(dplyr)

  datapath <- "D:/Projects/D2P 2021/Data"
  
  
# Read --------------------------------------------------------------------

  # Read All Fire Data
  fire_sf <- sf::st_read(file.path(datapath, "Raw Data/Historic_GeoMAC_Perimeters_Combined_2000-2018-shp/US_HIST_FIRE_PERIMTRS_2000_2018_DD83.shp"))

  # River Basin Boundaries
  basins <- sf::st_read("D:/Projects/D2P 2021/Data/Raw Data/DIV3CO/DIV3CO.shp")
  
# Filter and Transform ----------------------------------------------------

  # Filter to Colorado
  co_fires <- fire_sf %>% dplyr::filter(state == "CO")
  
  
  # Calculate Season
  getSeason <- function(DATES) {
    WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-19",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-20",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-22",  format = "%Y-%m-%d") # Fall Equinox
    
    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))
    
    ifelse (d >= WS | d < SE, "Winter",
            ifelse (d >= SE & d < SS, "Spring",
                    ifelse (d >= SS & d < FE, "Summer", "Fall")))
  }
  
  co_fires$season <- getSeason(co_fires$perimeterd)

# Identify Major Basin ----------------------------------------------------

  # Consistent Map Projection
  basins <- basins %>% st_transform(4326)
  co_fires <- co_fires %>% st_transform(4326)
  
  # Find River Basin for each fire
  basins_mat <- st_intersects(basins, co_fires, sparse = F)
  basins_vec <- apply(basins_mat, 2, function(y) paste(basins$BASIN[y], collapse = "&"))
  
  co_fires$basin <- basins_vec
  
  
# Write Out ---------------------------------------------------------------

  st_write(co_fires, file.path(datapath, "SPatial Data/CO Fires.shp"), append = F)
  