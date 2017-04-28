#CODE APPENDIX 3
# THE FOLLOWING APPENDIX LOADS THE MAIN DATASET AND RUNS DESCRIPTIVE ANALYSIS AND DATA CHECKS

cat("\014") 
rm(list=ls())

#USER NOTE: YOU MUST ASSIGN A WORKING DIRECTORY FOR THE CODE TO FUNCTION
# The WD must have the main CSV file created in code appendix 2 in order to work. 
setwd("C:/Users/rbjoe/Dropbox/Kugejl/8. semester/Public Economics/Public_Economics/Data")

#Used packages 
#install.packages("readr")
#install.packages("dplyr")
#install.packages("xtable")

#The appendix has the following sections  
# 1. SHARE OF WORLD FDI FEATURED IN INDIVIDUAL COUNTRY DATA
# 2. HAVEN SHARE OF WORLD GDP
# 3. NUMBER OF DATA PAIRS
# 4. MISSING VALUES PER COUNTRY/YEAR for one variable
# 5. SIZE OF HAVENS
# 6. CHECK HAVEN VARIABLES

#Load data
library(readr)
df <- read_csv("10. Main Dataset.csv")

#####################################################################################################
# 1. SHARE OF WORLD FDI FEATURED IN INDIVIDUAL COUNTRY DATA
#####################################################################################################
  library("dplyr")
  
  #First we FDI incomes in indivudal
  worldshare <- 
    df %>%
    group_by(COU_label.en, obsTime) %>%
    summarise( 
              World = mean(World_FDIInc_Out_Total, na.rm=TRUE),
              Unallocated = mean(Unallocated_FDIInc_Out_Total, na.rm=TRUE),
              Allocated_Share = 100*round((World-Unallocated)/World, digits=2),
              totalFDI = sum(FDIInc_Out_Total,na.rm = TRUE), 
              total_share = 100*round(totalFDI / World, 2),
              Difference = (Allocated_Share - total_share),
              text = paste(total_share, " (",Allocated_Share,")", sep=""),
              totalFDIHaven = sum(FDIInc_Out_Total[Haven==1],na.rm = TRUE),
              Haven_share = 100*round(totalFDIHaven/totalFDI, 2),
              totalFDISmallHaven = sum(FDIInc_Out_Total[Small_Haven==1],na.rm = TRUE),
              SmallHaven_share = 100*round(totalFDISmallHaven/totalFDI, 2),
              totalFDILargeHaven = sum(FDIInc_Out_Total[Large_Haven==1],na.rm = TRUE),
              LargeHaven_share = 100*round(totalFDILargeHaven/totalFDI, 2)
              ) 
  
  #CREATE A TABLE WHICH SHOWS THE SHARE OF FDI INCOME REPORTED IN EACH COUNTRY
  sharetable <- worldshare[, colnames(worldshare) %in% c("COU_label.en", "obsTime", "text") ]
  library("tidyr")
  sharetable <- spread(sharetable, obsTime, text)
  sharetable <- arrange(sharetable,`2005`,`2006`,`2007`,`2008`,`2009`,`2010`,
                        `2011`, `2012`,`2013`,`2014`,`2015`,desc(COU_label.en))

  sharetable <- arrange(sharetable,COU_label.en)
    #Output table in latex
    library(xtable)
    xtable(sharetable) %>% print(include.rownames=FALSE)
   

  

  
#####################################################################################################
# 2. HAVEN SHARE OF WORLD GDP
#####################################################################################################
  
  countrygdp <- 
    df %>%
    group_by(COUNTERPART_AREA_label.en, obsTime) %>%
    summarise(
      GDP = mean(GDP_j,na.rm = TRUE),
      Haven = mean(Haven,na.rm = TRUE),
      Small_Haven = mean(Small_Haven,na.rm = TRUE),
      Large_Haven = mean(Large_Haven,na.rm = TRUE)
    )
    
    countrygdp$Type <- "Non-Haven"
    countrygdp$Type[countrygdp$Small_Haven==1] <- "Small Haven" 
    countrygdp$Type[countrygdp$Large_Haven==1] <- "Large Haven"
    
  

   
  
    #World GDP from http://databank.worldbank.org/data/reports.aspx?source=2&series=NY.GDP.MKTP.CD&country=
        obsTime <- c(2005,	2006,	2007,	2008,	2009,	2010,	2011,	2012,	2013,	2014,	2015)
        WorldGDP <- c(47390843776187, 51309491620617, 57754955756610,	63344394334398,	60043969657081,
                      65853629844043,	73174667698349,	74681983166315,	76783293108832,	78676353220866,	
                      74188701246151)
        worldgdp <- data.frame(obsTime, WorldGDP)
        worldgdp$WorldGDP <- worldgdp$WorldGDP*10^-6
        rm(obsTime, WorldGDP)
    
    #GDP for havens    
    havengdp <- 
          countrygdp %>%
          group_by(Haven, obsTime) %>%
          summarise(
            totalGDP = sum(GDP,na.rm = TRUE)
            
          ) %>%
          arrange(obsTime,Haven)
          rm(countrygdp)
          
          #Add world gdp        
          library(dplyr)
          havengdp <- left_join(havengdp, worldgdp)
          
          
          #GDP shares
          havengdp$Share <- 100*round(havengdp$totalGDP/havengdp$WorldGDP, 3)
    
      #GDP for havens by size 
          havengdpsize <- 
            countrygdp %>%
            group_by(Type, obsTime) %>%
            summarise(
              totalGDP = sum(GDP,na.rm = TRUE)
              
            ) %>%
            arrange(obsTime,Type)
          
          #Add world gdp        
          library(dplyr)
          havengdpsize <- left_join(havengdpsize, worldgdp)
          rm(worldgdp)
          
          #GDP shares
          havengdpsize$Share <- 100*round(havengdpsize$totalGDP/havengdpsize$WorldGDP, 3)
          
          

          
        
  
#####################################################################################################
# 3. NUMBER OF DATA PAIRS
#####################################################################################################


  library("dplyr")
  count <- 
    df %>%
    group_by(COU_label.en) %>%
    summarise(count = n_distinct(obsTime))
  
  # find all unique values 
  #for (x in colnames(df)) {
    #print(x)
    #unique(df[[x]]) %>% print()
    #print("   ")
    #rm(x)
  #}
  
  #table(unique(df$obsTime))


#####################################################################################################
# 4. MISSING VALUES PER COUNTRY/YEAR for one variable
##################################################################################################### 
  
var <- c("FDIInc_Inw_Total")

  mangler <- df %>%
    #filter(Haven ==1) %>%
    group_by(COU_label.en) %>%
    summarize(
      Missing_2005 = sum(is.na(var[obsTime == 2005])),
      Missing_2006 = sum(is.na(var[obsTime == 2006])),
      Missing_2007 = sum(is.na(var[obsTime == 2007])),
      Missing_2008 = sum(is.na(var[obsTime == 2008])),
      Missing_2009 = sum(is.na(var[obsTime == 2009])),
      Missing_2010 = sum(is.na(var[obsTime == 2010])),
      Missing_2011 = sum(is.na(var[obsTime == 2011])),
      Missing_2012 = sum(is.na(var[obsTime == 2012])),
      Missing_2013 = sum(is.na(var[obsTime == 2013])),
      Missing_2014 = sum(is.na(var[obsTime == 2014])),
      Missing_2015 = sum(is.na(var[obsTime == 2015])),
      Missing_2016 = sum(is.na(var[obsTime == 2016]))
    )
rm(var)


#####################################################################################################
# 5. SIZE OF HAVENS
##################################################################################################### 
  library("dplyr")
  
  size <- df %>%
          filter(Haven ==1) %>%
          group_by(COUNTERPART_AREA_label.en) %>%
          summarize(
            population = mean(pop_j, na.rm = TRUE)
          )

#####################################################################################################
# 6. CHECK HAVEN VARIABLES
#####################################################################################################  
  library("dplyr")
  
  havens <- df %>%
    #filter(Haven ==1) %>%
    group_by(COUNTERPART_AREA_label.en) %>%
    summarize(
      Haven = mean(Haven, na.rm = TRUE),
      All_havens = mean(All_havens, na.rm = TRUE),
      Small_Haven = mean(Small_Haven, na.rm=TRUE),
      Large_Haven = mean(Large_Haven, na.rm=TRUE),
      tjek = Small_Haven + Large_Haven
    ) %>%
    arrange(-All_havens)
    

  
  
