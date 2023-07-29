library(StMoMo); library(demography)

genderList <- c("male", "female"); agesFit <- 50:89; yearsFit <- 1960:1990

countryList <- c("BLR","AUT", "BEL", "FRATNP","IRL","ITA","JPN", "USA", "GBRTENW","CHE")

dataList <- function(ages, years, genderList, countryList)
  
{
  
  for (c in 1:length(countryList)) { for (g in 1:length(genderList))
    
  {
    
    nAg <- length(ages); nYr <- length(years)
    
    MorData <- lapply(countryList, function(x) hmd.mx(country = x, username = "salvatory@aims.ac.tz", password = "Salva=0606"))
    DataStMoMo <- lapply(1:length(MorData), function(i) lapply(genderList, function(x) StMoMoData(MorData[[i]], x)))

  }
    
  }
  
  return(DataStMoMo)
  
}

data <- dataList(agesFit, yearsFit, genderList, countryList)

fitModel <- function(data)
  
{
  LC <- lc(); APC <- apc(); CBD <- cbd(link = "log"); M7 <- m7(link = "log")
  RH <- rh(approxConst = TRUE); PLAT =  plat()
  models <- list(LC, RH, APC, CBD, M7, PLAT)
  modelNames <- c("LC","RH", "APC", "CBD", "M7", "PLAT")
  
  fitModels <- list()
  
  for (c in 1:length(countryList))
    
  {
    
  fitModels[[c]] <- lapply(1:length(genderList), function(g) lapply(models, function(x) fit(x, data = data[[c]][[g]], ages.fit = agesFit, years.fit = yearsFit)))
  
  }
  
  return(fitModels)
}

fitCountry <- fitModel(data)
