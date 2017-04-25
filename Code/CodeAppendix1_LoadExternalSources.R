cat("\014")  #Clear output
rm(list=ls()) #Clear stored elements

# CODE APPENDIX 1. 
# LOAD DATA FROM EXTERNAL SOURCES
#The following appendix loads data from external sources, and saves them as CSV files in the working directory


#Used packages
#install.packages("rsdmx")
#install.packages("wbstats")
#install.packages("dplyr")

#USER NOTE: YOU MUST ASSIGN A WORKING DIRECTORY FOR THE CODE TO FUNCTION
setwd("C:/Users/rbjoe/Dropbox/Kugejl/8. semester/Public Economics/Public_Economics/Data")

#################################################################################################################

#The appendix has the following sections  
  #1. FDI INCOME BY PARTNER COUNTRY FROM OECD.   
  #2. GROSS NATIONAL INCOME FROM OECD
  #3. TAX RATES FROM OECD 
  #4. GDP from World Bank
  #5. EXCHANGE RATES FROM OECD

#Remaining datasources were created or downloaded manually, but are available from github. 
#All data sources (including those here created) are loaded automatically in the next appendix. 


#################################################################################################################
#1. FDI INCOME BY PARTNER COUNTRY FROM OECD.  
#################################################################################################################
#Note that the code takes quite a while to run

  #Download Dataset
  library("rsdmx")
  sdmx <- readSDMX(providerId = "OECD", resource = "data", flowRef = "FDI_INC_CTRY",
                   key = list("AUS+AUT+BEL+CAN+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ITA+JPN+KOR+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA", "USD", NULL), 
                   start = 1900, end = 2016,
                   dsd = TRUE)
  df <- as.data.frame(sdmx, labels = TRUE)
  rm(sdmx)
  
    #metadata
    dsd <- readSDMX("http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/FDI_INC_CTRY")
    cls <- slot(dsd, "codelists")
    rm(dsd)
  
  write.csv(df, file="1. FDI Income (OECD).csv", row.names = FALSE)
  
  #Create the zip file manually. 
  #zip("1. FDI Income (OECD).zip", "1. FDI Income (OECD).csv")

  
#################################################################################################################
#2. GROSS NATIONAL INCOME FROM OECD
#################################################################################################################
library("rsdmx")
sdmx <- readSDMX(providerId = "OECD", resource = "data", flowRef = "SNA_TABLE2",
                 key = list("AUS+AUT+BEL+CAN+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ITA+JPN+KOR+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA", NULL), 
                 start = 2005, end = 2016,
                 dsd = TRUE)
inc <- as.data.frame(sdmx, labels = TRUE)
rm(sdmx)
write.csv(inc, file="2. National income (OECD).csv", row.names = FALSE)
  
  
#################################################################################################################  
#3. TAX RATES FROM OECD 
###############################################################################################################
  #Download Dataset
  library("rsdmx")
  sdmx <- readSDMX(providerId = "OECD", resource = "data", flowRef = "TABLE_II1",
                   key = list("AUS+AUT+BEL+CAN+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ITA+JPN+KOR+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA", NULL), 
                   start = 1900, end = 2016,
                   dsd = TRUE)
  tax <- as.data.frame(sdmx, labels = TRUE)
  rm(sdmx)
  write.csv(tax, file="3. Corporate tax rates (OECD).csv", row.names = FALSE)
  

#################################################################################################################
#4. GDP from World Bank
#################################################################################################################
library("wbstats")
#wbsearch <- wbsearch("GDP")

#Download data
gdp <- wb(country= "countries_only", indicator = "NY.GDP.MKTP.CD", 
          startdate = 2005, enddate = 2016, removeNA = FALSE)

#Get ISO3C codes.
countries <- wbcountries(lang ="en")
countries <- countries[, colnames(countries) %in% c("iso3c", "iso2c", "country")]
library("dplyr")
gdp <- left_join(gdp, countries)
rm(countries)

#Export data
write.csv(gdp, file="5. GDP (World Bank).csv", row.names = FALSE) #Number follows appendix 2

  
#################################################################################################################
#5. EXCHANGE RATES FROM OECD
#################################################################################################################
  library("rsdmx")
  sdmx <- readSDMX(providerId = "OECD", resource = "data", flowRef = "SNA_TABLE4",
                   key = list("AUS+AUT+BEL+CAN+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ITA+JPN+KOR+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA", 
                              NULL), 
                   start = 2005, end = 2016,
                   dsd = TRUE)
  exc <- as.data.frame(sdmx, labels = TRUE)
  rm(sdmx)
  write.csv(exc, file="7. Exchange rates (OECD).csv", row.names = FALSE) #numbering follows appendix 2
  

###########################################################################  
#END
###########################################################################  
  
  