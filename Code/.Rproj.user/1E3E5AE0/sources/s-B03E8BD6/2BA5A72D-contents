## Model

# Setup -------------------------------------------------------------------

  options(scipen = 999, stringsAsFactors = F)

  library(dplyr); library(readr); library(leaps); library(car); library(ggplot2);
  library(caret)
  
  datapath <- "D:/Projects/D2P 2021/Data"


# Read --------------------------------------------------------------------

  waterData <- readr::read_csv(file.path(datapath, "Modeling Dataset.csv"))

  
# Create Regressors -------------------------------------------------------

  ## Log and sqrt transformations need to shift to account for negative values
  
  ## Log Flow
  waterData$lflow <- log(waterData$flow)
  ## Log Precip
  waterData$lprcp <- log(waterData$prcp + 1)
  
  b <- median(waterData$tmax) # Beta value for logistic function, air temp at inflection point
  g <- .07 # gamma value for logistic function
  
  waterData$Stmax <- max(waterData$tmax) / (1 + exp(g*(b-waterData$tmax)))

  ## Code factors
  waterData$basin <- as.factor(waterData$basin)
  
  ## Log basin acres burned, shift to account for negative
  waterData$lacres_basin <- log(waterData$acres_basin + 1)
  

# Model -------------------------------------------------------------------

  mod1 <- lm(Wtemp ~ yr + flow + prcp + tmax + alt + acres_basin + drain + basin, data = waterData)
  

# Diagnostics -------------------------------------------------------------

  dev.off()
  plot(mod1$fitted.values, mod1$model$Wtemp)
  
  car::residualPlots(mod1)
  ## Systematic nonlinear relationships for flow, prcp, tmax, and finally for fitted values
  
  
  car::crPlots(mod1)
  ## Similarly, nonlinear relationships for flow, prcp, and tmax
  
  ## Refit model with logs
  mod2 <- lm(Wtemp ~ yr + lflow + lprcp + tmax + alt + acres_basin + ldrain + basin, data = waterData)
  
  plot(mod2$fitted.values, mod2$model$Wtemp)
  
  car::residualPlots(mod2)
  car::crPlots(mod2)
  
  
  
  plot(mod2)
  
  
  ## Refit model with S curve in temp
  mod3 <- lm(Wtemp ~ yr + lflow + lprcp + Stmax + alt + acres_basin + ldrain + basin, data = waterData)
  
  plot(mod3$fitted.values, mod3$model$Wtemp)
  
  car::residualPlots(mod3)
  car::crPlots(mod3)
  
  
  # plot(mod3)
  
  marginalModelPlots(mod3)
  
  
  
  ### Quadratic term for temp, FINAL MODEL
  waterData$acres1000 <- waterData$acres_basin / 1000
  waterData$alt100 <- waterData$alt / 100
  waterData$drain100 <- waterData$drain / 100
  mod22 <- lm(Wtemp ~ yr + lflow + lprcp + poly(tmax, degree = 2) + alt100 + acres1000 + drain100 + basin, data = waterData)
  summary(mod22)
  plot(mod22$fitted.values, mod22$model$Wtemp)
  
  df_temp <- data.frame(
    "Observed Values" = mod22$model$Wtemp,
    "Fitted Values" = mod22$fitted.values,
    check.names = F
  )
  
  df_temp %>% ggplot(aes(`Fitted Values`, `Observed Values`)) + 
    geom_point() +
    labs(title = "Observed vs Fitted Values") +
    theme_minimal()
  
  car::residualPlots(mod22)
  plot(mod22, which = 1)
  
  confint(mod22)
  
  

# Check Influential / Outliers --------------------------------------------
  
  # Leverage Points
  infIndexPlot(mod22, vars = "hat")
  
  # plot(mod22, which = 4)
  
  # Outliers
  outlierTest(mod22, cutoff=Inf, n.max = 2)
  
  # Influence
  dev.off()
  car::influencePlot(mod22)
  
  # car::dfbetasPlots(mod2)
  
  ## Examine Influence of Observation 11 and 1229
  

# Check Regression Assumptions --------------------------------------------

  plot(mod22, which = 1)
  
  
  shapiro.test(mod22$residuals)
  
  
  plot(mod22$residuals, mod2$fitted.values)
  
  
  
  library(effects)
  effects::predictorEffects(mod2)
  
  plot(effects::predictorEffects(mod2))

# Inference ---------------------------------------------------------------

  # Regression Coef Confidence Intervals
  confint(mod22)
  
  
  
  
  
  