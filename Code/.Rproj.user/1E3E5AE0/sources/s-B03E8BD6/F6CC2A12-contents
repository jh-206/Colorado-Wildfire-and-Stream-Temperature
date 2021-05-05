## Model

# Setup -------------------------------------------------------------------

  options(scipen = 999, stringsAsFactors = F)

  library(dplyr); library(readr); library(leaps); library(car); library(ggplot2);
  library(caret)
  
  datapath <- "D:/Projects/D2P 2021/Data"

# Read --------------------------------------------------------------------

  waterData <- readr::read_csv(file.path(datapath, "Modeling Dataset.csv"))

  
# Create Regressors -------------------------------------------------------

  ## Log fire vars, shift 1 unit to account for log(0)
  waterData$lacres_state <- log(waterData$acres_state + 1)
  waterData$lnum_fires_state <- log(waterData$num_fires_state + 1)
  waterData$lacres_basin <- log(waterData$acres_basin + 1)
  waterData$lnum_fires_basin <- log(waterData$num_fires_basin + 1)
  
  ## Log Flow
  waterData$lflow <- log(waterData$flow)
  ## Log Precip
  waterData$lprcp <- log(waterData$prcp + 1)
  ## Log Drainage Area
  waterData$ldrain <- log(waterData$drain)

# Visualize ---------------------------------------------------------------

  # Response Variable
  temp_quants <- round(summary(waterData$Wtemp)[c("Min.", "Mean", "Max.")], 2)
  quant_labs <- paste(c("Min: ", "Mean: ", "Max: "), temp_quants, sep = "")
  
  # waterData %>% 
  #   ggplot(aes(x = Wtemp)) + 
  #   labs(title = "Colorado Seasonal Average Water Temperature Distribution (2000 - 2018)", x = "Water Temperature (C)") +
  #   geom_density(color = "blue", fill = "lightblue", alpha = .4) + 
  #   geom_vline(xintercept = temp_quants, 
  #              linetype = "dashed", color = "blue", alpha = .4) +
  #   xlim(c(-5, 25)) +
  #   annotate(geom = "text", x = temp_quants, 
  #            y = 0.01, label = quant_labs, color = "blue", vjust = -.3,
  #            size = 4, angle = 90, alpha = .8) +
  #   theme_minimal()
  
  # Relationship of Variables with Basin
  waterData %>% 
    ggplot(aes(x = basin, y = Wtemp)) +
    labs(title = "Water Temperature Distribution by River Basin", x = "River Basin", y = "Water Temperature (C)") +
    geom_boxplot() +
    theme_minimal()
  
  scales::hue_pal()(4)
  # Relationship with Water Temp
  ## Air Temp
  # waterData %>% 
  #   ggplot(aes(x = tmax, y = Wtemp, color = season)) + 
  #   scale_colour_manual(values = c("#F8766D", "#b3be3c", "#39B600", "#00BFC4")) +
  #   labs(title = "Air Temperature versus Water Temperature", x = "Average Maximum Daily Air Temperature (C)", y = "Water Temp (C)") +
  #   geom_point(size = 1.5) + 
  #   theme_minimal()
  
  ## Flow
  # waterData %>% 
  #   ggplot(aes(x = lflow, y = Wtemp, color = season)) + 
  #   scale_colour_manual(values = c("#F8766D", "#b3be3c", "#39B600", "#00BFC4")) +
  #   labs(title = "Log Discharge versus Water Temperature", x = "Discharge", y = "Water Temp (C)") +
  #   geom_point(size = 1.5) +
  #   theme_minimal()
  ## Altitude, average by site
  # waterData %>% 
  #   group_by(site_no) %>% summarise(alt = unique(alt), meantemp = mean(Wtemp)) %>% 
  #   ggplot(aes(x = alt, y = meantemp)) + 
  #   labs(title = "Altitude vs Average Annual Temp", x = "Site Altitude (ft)", y = "Average Annual Water Temp (C)") +
  #   scale_x_continuous(labels = scales::comma) +
  #   geom_point(size = 2, color = "#00BFC4") + 
  #   theme_minimal()
  ## Longitude and Altitude Strongly related
  # waterData %>% 
  #   ggplot(aes(x = long, y = alt)) + 
  #   labs(title = "Longitude vs Altitude", x = "Longitude", y = "Altitude (ft)") +
  #   geom_point(size = 2, color = "#C77CFF") + 
  #   theme_minimal()
  
  
  ## Precip
  waterData %>% 
    ggplot(aes(x = prcp, y = Wtemp, color = season)) + 
    labs(title = "Precipitation versus Water Temperature", x = "Log Daily Precipitation (mm)", y = "Water Temp (C)") +
    geom_point(size = 1.5) + 
    theme_minimal()
  waterData %>% 
    ggplot(aes(x = lprcp, y = Wtemp, color = season)) + 
    labs(title = "Log Precipitation versus Water Temperature", x = "Log Daily Precipitation (mm)", y = "Water Temp (C)") +
    geom_point(size = 1.5) + 
    theme_minimal()
  
  ## Drainage Area
  # waterData %>% 
  #   group_by(site_no) %>% summarise(ldrain = unique(ldrain), meantemp = mean(Wtemp)) %>%
  #   ggplot(aes(x = ldrain, y = meantemp)) +
  #   labs(title = "Log Drainage Acreage vs Mean Site Water Temperature", x = "Log Drainage Acreage", y = "Mean Site Water Temperature") +
  #   geom_point(size = 1.5) + 
  #   theme_minimal()
  
  
  
  # Relationship w Fire Data
  
  ## Number of Fires
  waterData %>% 
    ggplot(aes(x = acres_basin, y = Wtemp, color = season)) +
    geom_point() +
    scale_colour_manual(values = c("#F8766D", "#b3be3c", "#39B600", "#00BFC4")) +
    labs(x = "Number of Wildfires", y = "Water Temp (C)", 
         title = "Number of Wildfires in River Basin vs Seasonal Water Temperature") +
    theme_minimal()
  ## Acres Burned
  waterData %>% 
    ggplot(aes(x = lacres_basin, y = Wtemp, color = season)) +
    geom_point() +
    scale_colour_manual(values = c("#F8766D", "#b3be3c", "#39B600", "#00BFC4")) +
    labs(x = "Log Acres Burned", y = "Water Temp (C)", 
         title = "Log Total Acres Burned in River Basin vs Seasonal Water Temperature") +
    theme_minimal()

  

# Code as Factors ---------------------------------------------------------

  waterData$basin <- as.factor(waterData$basin)
  waterData$season <- as.factor(waterData$season)
  

# Collinearity Checks -----------------------------------------------------

  # Specify model with all variables
  full_spec <-  Wtemp ~ yr + season + flow + prcp + tmax + alt + drain + basin + acres_basin
  mod1 <- lm(full_spec, data = waterData)
  
  # Check VIF
  car::vif(mod1)
  
  
  spec3 <-  Wtemp ~ yr + flow + prcp + tmax + alt + drain + basin + acres_basin
  mod3 <- lm(spec3, data = waterData)
  
  car::vif(mod3)
  
  ## Season is highly related to flow, precipitation, wildfires, and air temperature. 
  ## Removing season from the model massively reduces collinearity in the model
  
  

# Variable Selection ------------------------------------------------------

  ## Running Exhaustive model search without categorical Basin variable, then compare with basin
  ## wildfire variable is variable of research interest
  
  mods <- regsubsets(Wtemp ~ yr + flow + prcp + tmax + alt + drain + acres_basin, 
                     intercept = T,
                     data = waterData, method = "exhaustive")
  
  modsummary <- summary(mods)
  
  ## Best model Metrics, plus 1 on indices due to force.in argument
  
  ## BIC
  pbest <- which.min(modsummary$bic) # model size with lowest BIC
  plot(1:length(modsummary$bic), modsummary$bic, xlab = "Number of Predictors", ylab = "BIC", type = "b")
  points(pbest, modsummary$bic[pbest], col = "red", pch = 16)
  
  vars <- modsummary$which[pbest, ] # variables used in best BIC model
  names(modsummary$which[pbest, vars])[-1]
  
  
  ## 10-Fold Cross Validation for models with 6 and 7 predictors
  cv_10fold <- caret::trainControl(method = "cv", number = 10)
  
  spec6 <- Wtemp ~ yr + flow + prcp + tmax + alt + drain
  spec7 <- Wtemp ~ yr + flow + prcp + tmax + alt + drain + acres_basin
  
  set.seed(5387) # set seed for reproducibility, arbitrarily use class number
  mod6 <- train(spec6, data = waterData, trControl = cv_10fold, 
                 method = "lm")
  mod7 <- train(spec7, data = waterData, trControl = cv_10fold, 
                 method = "lm")
  
  # Compare Root mean squared error and mean absolute errors
  mod6$results[c("RMSE", "MAE")]
  mod7$results[c("RMSE", "MAE")]
  
  
  # Compare adding Basin
  specB <- Wtemp ~ yr + flow + prcp + tmax + alt + acres_basin + drain + basin
  modB <- train(specB, data = waterData, trControl = cv_10fold, 
                method = "lm")
  
  modB$results[c("RMSE", "MAE")]
  
  ## Adding river basin slightly improves CV metrics, check AIC
  mod6 <- lm(Wtemp ~ yr + flow + prcp + tmax + alt + drain, data = waterData)
  mod7 <- lm(Wtemp ~ yr + flow + prcp + tmax + alt + acres_basin + drain, data = waterData)
  modB <- lm(Wtemp ~ yr + flow + prcp + tmax + alt + acres_basin + drain + basin, data = waterData)
  
  aic(mod6); aic(mod7); aic(modB)
  
  ## AIC prefers model with 7 predictors plus Basin, so all of the variables retained
  
  