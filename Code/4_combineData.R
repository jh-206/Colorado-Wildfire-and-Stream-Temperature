## 4. Transform Datasets and join

# Setup -------------------------------------------------------------------

  options(scipen = 999, stringsAsFactors = F)

  library(dplyr); library(readr); library(stringr); library(tidyr); library(sf)
  
  datapath <- "D:/Projects/D2P 2021/Data"


# Read --------------------------------------------------------------------

  climateData <- readr::read_csv(file.path(datapath, "PRISM Climate Data.csv"))
  waterData <- readr::read_csv(file.path(datapath, "USGS CO Temp Data.csv"))
  siteData <- readr::read_csv(file.path(datapath, "USGS CO Phys Sites.csv"))
  fireData <- sf::st_read("D:/Projects/D2P 2021/Data/Spatial Data/CO Fires.shp")
  basins <- sf::st_read("D:/Projects/D2P 2021/Data/Raw Data/DIV3CO/DIV3CO.shp")
  

# Transform ---------------------------------------------------------------

  # Filter to Relevant Time Period
  waterData <- waterData[which(waterData$Date <= max(fireData$perimeterd)),]
  # Calculate Season and Year
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
  
  # Add year and season, filter to complete years of observations
  waterData$season <- getSeason(waterData$Date)
  waterData$yr <- lubridate::year(waterData$Date)
  sts <- waterData %>% group_by(site_no) %>% summarise(nseas = n_distinct(season))
  sts <- sts[which(sts$nseas == 4),]
  waterData <- waterData[which(waterData$site_no %in% sts$site_no),]
  
  # Precipitation Data
  prcp <- climateData %>% 
    dplyr::select(date, contains("prcp")) %>% 
    tidyr::pivot_longer(cols = -date, names_to = "site_no", values_to = "prcp") %>% 
    dplyr::mutate(site_no = gsub(".*?([0-9]+).*", "\\1", site_no)) %>% 
    dplyr::rename(Date = date) %>% 
    dplyr::filter(site_no %in% waterData$site_no)
  
  # Max Temperature Data
  tmax <- climateData %>% 
    dplyr::select(date, contains("tmax")) %>% 
    tidyr::pivot_longer(cols = -date, names_to = "site_no", values_to = "tmax") %>% 
    dplyr::mutate(site_no = gsub(".*?([0-9]+).*", "\\1", site_no)) %>% 
    dplyr::rename(Date = date) %>% 
    dplyr::filter(site_no %in% waterData$site_no)
  
  # Join Temp and Precip
  df1 <- dplyr::left_join(
    waterData,
    prcp,
    by = c("site_no", "Date")
  )
  
  df1 <- dplyr::left_join(
    df1,
    tmax,
    by = c("site_no", "Date")
  )
  
  # Add Basin to Site
  basins <- basins %>% st_transform(crs = 4326)
  site_sf <- siteData %>% 
    st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326)
  basins_mat <- st_intersects(basins, site_sf, sparse = F)
  basins_vec <- apply(basins_mat, 2, function(y) paste(basins$BASIN[y], collapse = "&"))
  siteData$basin <- basins_vec
  
  
  # Join Site Info
  df1 <- dplyr::left_join(
    df1,
    siteData %>% 
      dplyr::select(site_no, alt_va, dec_lat_va, dec_long_va, drain_area_va, basin),
    by = "site_no"
  )

  
  
  # Summarize fire data by season, year, and basin
  fire_df <- fireData %>% as.data.frame()
  ## State totals
  fire1 <- fire_df %>% group_by(fireyear, season) %>% summarise(num_fires_state = n(), acres_state = sum(gisacres)) 
  ## Handle fires that straddled basins, duplicate rows
  ids <- which(grepl(fire_df$basin, pattern = "\\&")) # row indices to be duplicated
  fire_df2 <- fire_df[ids,] # first half of &
  fire_df2$basin <- gsub(pattern = "&.*", replacement = "", x = fire_df2$basin)
  fire_df <- bind_rows(fire_df, fire_df2) # Add rows
  fire_df$basin[ids] <- gsub(pattern = ".*&", replacement = "", x = fire_df$basin[ids])# second half of &
  ## Basin Totals
  fire2 <- fire_df %>% group_by(fireyear, season, basin) %>% summarise(num_fires_basin = n(), acres_basin = sum(gisacres)) # Grouped by Basin
  
  
  # Potential modeling variables
  v <- c("Wtemp", "Flow", "prcp", "tmax", "alt_va", "dec_lat_va", "dec_long_va", "drain_area_va")
  # Take seasonal averages per year of quantitative varibles above
  df2 <- df1 %>% 
    group_by(site_no, yr, season) %>% 
    summarise_at(.vars = v, mean)
  # Ensure complete climate data 
  df2 <- df2[!is.na(df2$prcp) & !is.na(df2$tmax),]
  # Add basin label back
  df2 <- left_join(df2, df1 %>% group_by(site_no) %>% summarise(basin = unique(basin)), by = "site_no")
  
  # Add fire data for state
  df2 <- dplyr::left_join(
    df2 %>% filter(yr <= max(fire_df$fireyear)),
    fire1,
    by = c("season" = "season", "yr" = "fireyear")
  )
  
  # Add fire data separated by basin
  df2 <- dplyr::left_join(
    df2 %>% filter(yr <= max(fire_df$fireyear)),
    fire2,
    by = c("season" = "season", "yr" = "fireyear", "basin" = "basin")
    )

  # Clean
  ## Remove negative flow, not physically possible
  df2 <- df2[-which(df2$Flow <= 0),]
  ## Code missing fire as zero acres and num
  df2$num_fires_state[is.na(df2$num_fires_state)] <- 0
  df2$num_fires_basin[is.na(df2$num_fires_basin)] <- 0
  df2$acres_state[is.na(df2$acres_state)] <- 0
  df2$acres_basin[is.na(df2$acres_basin)] <- 0
  

# Consistent, Informative Variable Names ----------------------------------

  df2 <- df2 %>% 
    dplyr::rename(
      flow = Flow,
      alt = alt_va,
      lat = dec_lat_va,
      long = dec_long_va,
      drain = drain_area_va
    )
  
  
# Write Output ------------------------------------------------------------

  
  readr::write_csv(df2, file.path(datapath, "Modeling Dataset.csv"))
  

  