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

#The appendix has the following sections  

#Load data
library(readr)
df <- read_csv("10. Main Dataset.csv")

#####################################################################################################
# SHARE OF WORLD GDP FEATURED IN INDIVIDUAL COUNTRY DATA
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
              text = paste(total_share, " (", Allocated_Share, ")")
              )
  
  sharetable <- worldshare[, colnames(worldshare) %in% c("COU_label.en", "obsTime", "text") ]
  library("tidyr")
  sharetable <- spread(sharetable, obsTime, text)
  
#####################################################################################################
#NUMBER OF DATA PAIRS
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
# MISSING VALUES PER COUNTRY/YEAR for one variable
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
# SIZE OF HAVENS
##################################################################################################### 
  library("dplyr")
  
  size <- df %>%
          filter(Haven ==1) %>%
          group_by(COUNTERPART_AREA_label.en) %>%
          summarize(
            population = mean(pop_j, na.rm = TRUE)
          )

#####################################################################################################
# Haven variables
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
    
#####################################################################################################
# SHARE IN HAVENS
#####################################################################################################  

  
  library("dplyr")
  
  relevant <- c("ID", "COU_label.en", "COUNTERPART_AREA_label.en", "DI_T_D4P_F", "DO_T_D4P_F", "Haven")
  
  shares <- df %>%
    #filter(df$obsTime == 2014) %>%
    group_by(COU_label.en, obsTime) %>%
    summarise(
      Primary_Income_Receivable_World = mean(D1_D4FRS2, na.rm = TRUE),
      Primary_Income_Payable_World = mean(D1_D4TOS2, na.rm = TRUE),
      Primary_Income_Net_World = mean(D1_D4NFRS2, na.rm = TRUE),
      FDI_Income_Total_Outward = sum(DO_T_D4P_F, na.rm = TRUE),
      Haven_FDI_Income_Outward = sum(DO_T_D4P_F[Haven==1], na.rm = TRUE)/sum(DO_T_D4P_F, na.rm = TRUE),
      NonHaven_FDI_Income_Outward = sum(DO_T_D4P_F[Haven==0], na.rm = TRUE)/sum(DO_T_D4P_F, na.rm = TRUE),
      FDI_Income_Total_Inward = sum(DI_T_D4P_F, na.rm = TRUE),
      Haven_FDI_Income_Inward = sum(DI_T_D4P_F[Haven==1], na.rm = TRUE)/sum(DI_T_D4P_F, na.rm = TRUE),
      NonHaven_FDI_Income_Inward = sum(DI_T_D4P_F[Haven==0], na.rm = TRUE)/sum(DI_T_D4P_F, na.rm = TRUE)
    ) #%>%
    #arrange(-Primary_Income_Receivable_World)
  
  shares$FDIOutward_Receivable <- shares$FDI_Income_Total_Outward / shares$Primary_Income_Receivable_World
  shares$FDIInward_Payable <- shares$FDI_Income_Total_Inward / shares$Primary_Income_Payable_World
  
  write.csv(shares, file="shares", row.names = FALSE)

  ##################################################################################################### 
  
  
