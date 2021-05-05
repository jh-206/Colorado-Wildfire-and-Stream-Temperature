## Get USGS Stream Info

# Setup -------------------------------------------------------------------

  options(scipen = 999, stringsAsFactors = F)
  
  library(dataRetrieval); library(readxl); library(xlsx)
  
  datapath <- "D:/Projects/D2P 2021/Data"
  
  
  # Parameter codes
  parameterCd <- c("00010", "00060")  # 00010 temp, 00060 discharge
  
  # Stat code
  statCd <- c("00003", "00001")  # Mean and maximum
  
  # Data Retrieval Dates
  startDate <- "2000-01-01"
  endDate <- "2021-03-10"

# Get Data ----------------------------------------------------------------

  # Find sites in Colorado with given data
  sites <- dataRetrieval::whatNWISsites(stateCd="CO",
                                        parameterCd=parameterCd,
                                        hasDataTypeCd = "dv")
  # Filter to stream sites
  sites <- sites[which(sites$site_tp_cd == "ST"),]
  
  # Info on parameter codes
  dataRetrieval::readNWISpCode(parameterCd)
  
  
  # Get Data For all Codes, rename and filter out NA values
  waterData <- readNWISdv(sites$site_no, parameterCd, 
                   startDate, endDate, statCd=statCd)
  waterData <- renameNWISColumns(waterData)
  
  waterData <- waterData[!is.na(waterData$Wtemp) & !is.na(waterData$Flow),]
  
  
  
  siteData <- dataRetrieval::readNWISsite(unique(waterData$site_no))
  
  
  
  readr::write_csv(waterData, file.path(datapath, "USGS CO Temp Data.csv"))
  readr::write_csv(siteData, file.path(datapath, "USGS CO Phys Sites.csv"))
  
  
  
  
  
  
  
  
  
  