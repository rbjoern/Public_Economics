#CODE APPENDIX 2
# THE FOLLOWING APPENDIX LOADS DATA FROM CSV FILES, AND FORMATS THE DATA SET PROPERLY

cat("\014") 
rm(list=ls())

#Used packages
#install.packages("readr")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("foreign")
#install.packages("zoo")

#USER NOTE: YOU MUST ASSIGN A WORKING DIRECTORY FOR THE CODE TO FUNCTION
# The WD must have the CSV files created in code appendix 1 in order to work. 
setwd("C:/Users/rbjoe/Dropbox/Kugejl/8. semester/Public Economics/Public_Economics/Data")

#USER NOTE: All datasets from github must be saved in the working directory for the code to run. 

#The appendix has the following sections  
  #1. FDI INCOME BY PARTNER COUNTRY FROM OECD. 
  #2. NATIONAL INCOME FROM OECD
  #3. TAX RATES FOR OECD COUNTRIES
  #4. GRAVITY DATA FROM CEPII
  #5. GDP DATA FROM WORLD BANK  
  #6. LIST OF TAX HAVENS
  #7. EXCHANGE RATES FOR OECD COUNTRIES
  #8. CFC Rules Data
  #9. TAX RATES FROM KPMG
  #10. FINAL CLEANING OF DATA & EXPORT

############################################################################
#1. FDI INCOME BY PARTNER COUNTRY FROM OECD. 
############################################################################
  #After this step we have a dataset with an observation for each partner country in each year
  #The only information in this dataset this far will be various measures of FDI income. 
  
  #LOAD DATA
    #Load df from csv file
    library(readr)
    df <- read_csv("1. FDI Income (OECD).csv")
  
    
  #FILTER RELEVANT OBSERVATRIONS  
    #Filter: All resident units (as opposed to SPEs and non-SPEs respectively)
    #We end up needing these later, as we replace missing totals with non-SPEs
    #df <- subset(df, TYPE_ENTITY == "ALL")
    
    #Temporary: Work only with denmark 2015
    #df <- subset(df, COU=="DNK")
    #df <- subset(df, obsTime == "2015")
    
    #We don't filter away confidential values, since there is no need. 
    #Filter out confidential values 
      #Confidential values are saved as missing which is fine for our analysis, but their presence
      # messes up the spreading of the data below, so we simply drop the rows
      #Obsstatus is either d for Secondary Confidentiality, C for Non-publishable and confidential value
      # or simply missing if the observations is fine 
      #df <- subset(df, is.na(OBS_STATUS)) #not confidential
      #Note that the rows are still there with missing values, we just remove the duplicate rows showing theyre missing. 
    
    #Remove observations which are not countries
      #World and World unallocated are removed below, after they're added to each COU in a variable. 
    noncountries <- c(
      #"WORLD",
      "WORLD Excluding OECD countries",
      #"WORLD unallocated and confidential",
      "OECD",
      "EUROPE",
      "EUROPE Excluding OECD countries",
      "AFRICA",
      "Northern Africa",
      "Other African countries",
      "AMERICA",
      "AMERICA Excluding OECD countries",
      "Northern America",
      "Northern America Excluding OECD countries",
      "Central America and Caribbean countries",
      "Central America and Caribbean excluding OECD countries",
      "South America",
      "South America Excluding OECD",
      "Other territories",
      "ASIA",
      "ASIA Excluding OECD countries",
      "Near and Middle East",
      "Near and Middle East Excluding OECD countries",
      "Other Near and Middle East (Western Asia)",
      "Other NME Excluding OECD  countries",
      "Other Asian countries",
      "Other Asian countries excluding OECD countries",
      "AUSTRALIA, OCEANIA AND POLAR REGIONS",
      "Australia, Oceania and Polar regions  Excluding OECD countries",
      "EU28",
      "EU27",
      "EU25",
      "ASEAN countries",
      "G20 countries excl. European Union",
      "G20 OECD countries",
      "G20 Non-OECD countries",
      "MENA countries",
      "Total Official Development Assistance (ODA) recipients",
      "ODA recipients -Europe",
      "ODA recipients -Africa",
      "ODA recipients-America",
      "ODA recipients-Asia",
      "ODA recipients-Oceania",
      "EU15",
      "Gulf Arabian countries"
    )
    df <- subset(df, !(COUNTERPART_AREA_label.en %in% noncountries) )
    rm(noncountries) #remove
    
  
  #FILTER RELEVANT COLUMNS
    #THe dataset has both english and french labels. We drop the french ones. 
    df <- df[, -grep(pattern = ".fr", colnames(df))] #Drops variables ending in .fr
    
    #Remove columns with more than one possible value for each country pair (except the main one)
      #Create ID variable, which combines the information into one variable
      df$Flow <- paste(df$MEASURE_PRINCIPLE, df$FDI_TYPE,df$TYPE_ENTITY, sep = "_")
      
      #Rename the variables
      #All entities
      df$Flow[df$Flow=="DO_T_D4P_F_ALL"]    <- "FDIInc_Out_Total"
      df$Flow[df$Flow=="DI_T_D4P_F_ALL"]    <- "FDIInc_Inw_Total"
      df$Flow[df$Flow=="DO_T_D42S_F5_ALL"]  <- "FDIInc_Out_Dividends"
      df$Flow[df$Flow=="DI_T_D42S_F5_ALL"]  <- "FDIInc_Inw_Dividends"
      df$Flow[df$Flow=="DO_T_D4S_F5_ALL"]   <- "FDIInc_Out_Equity"
      df$Flow[df$Flow=="DI_T_D4S_F5_ALL"]   <- "FDIInc_Inw_Equity"
      df$Flow[df$Flow=="DO_T_D4Q_FL_ALL"]   <- "FDIInc_Out_Debt"
      df$Flow[df$Flow=="DI_T_D4Q_FL_ALL"]   <- "FDIInc_Inw_Debt"
      df$Flow[df$Flow=="DO_T_D43S_F5_ALL"]  <- "FDIInc_Out_Reinvested"
      df$Flow[df$Flow=="DI_T_D43S_F5_ALL"]  <- "FDIInc_Inw_Reinvested"
      
      #SPE's
      df$Flow[df$Flow=="DO_T_D4P_F_RSP"]    <- "FDIInc_Out_Total_SPE"
      df$Flow[df$Flow=="DI_T_D4P_F_RSP"]    <- "FDIInc_Inw_Total_SPE"
      df$Flow[df$Flow=="DO_T_D42S_F5_RSP"]  <- "FDIInc_Out_Dividends_SPE"
      df$Flow[df$Flow=="DI_T_D42S_F5_RSP"]  <- "FDIInc_Inw_Dividends_SPE"
      df$Flow[df$Flow=="DO_T_D4S_F5_RSP"]   <- "FDIInc_Out_Equity_SPE"
      df$Flow[df$Flow=="DI_T_D4S_F5_RSP"]   <- "FDIInc_Inw_Equity_SPE"
      df$Flow[df$Flow=="DO_T_D4Q_FL_RSP"]   <- "FDIInc_Out_Debt_SPE"
      df$Flow[df$Flow=="DI_T_D4Q_FL_RSP"]   <- "FDIInc_Inw_Debt_SPE"
      df$Flow[df$Flow=="DO_T_D43S_F5_RSP"]  <- "FDIInc_Out_Reinvested_SPE"
      df$Flow[df$Flow=="DI_T_D43S_F5_RSP"]  <- "FDIInc_Inw_Reinvested_SPE"
      
      #Resident Operating Units (Non-SPEs)
      df$Flow[df$Flow=="DO_T_D4P_F_ROU"]    <- "FDIInc_Out_Total_NSPE"
      df$Flow[df$Flow=="DI_T_D4P_F_ROU"]    <- "FDIInc_Inw_Total_NSPE"
      df$Flow[df$Flow=="DO_T_D42S_F5_ROU"]  <- "FDIInc_Out_Dividends_NSPE"
      df$Flow[df$Flow=="DI_T_D42S_F5_ROU"]  <- "FDIInc_Inw_Dividends_NSPE"
      df$Flow[df$Flow=="DO_T_D4S_F5_ROU"]   <- "FDIInc_Out_Equity_NSPE"
      df$Flow[df$Flow=="DI_T_D4S_F5_ROU"]   <- "FDIInc_Inw_Equity_NSPE"
      df$Flow[df$Flow=="DO_T_D4Q_FL_ROU"]   <- "FDIInc_Out_Debt_NSPE"
      df$Flow[df$Flow=="DI_T_D4Q_FL_ROU"]   <- "FDIInc_Inw_Debt_NSPE"
      df$Flow[df$Flow=="DO_T_D43S_F5_ROU"]  <- "FDIInc_Out_Reinvested_NSPE"
      df$Flow[df$Flow=="DI_T_D43S_F5_ROU"]  <- "FDIInc_Inw_Reinvested_NSPE"
      
      
      #Save labels in separate dataset
      unnecessary <- c("MEASURE_PRINCIPLE", "FDI_TYPE", "MEASURE_PRINCIPLE_label.en",
                       "FDI_TYPE_label.en", "TYPE_ENTITY", "TYPE_ENTITY_label.en")
      stroemlabels <- unique(df[, c("Flow", unnecessary)])
      library(dplyr)
      stroemlabels <- arrange(stroemlabels, MEASURE_PRINCIPLE, FDI_TYPE_label.en )
      write.csv(stroemlabels, file="1.1. FDI Income Labels.csv", row.names = FALSE)
      
      #Drop variables now saved in separate dataset
      df <- df[, !(names(df) %in% unnecessary)]
      rm(unnecessary) #remove list from memory
      
    #Remove columns with only one possible value for all observations
      #Check that they only have one possible value (and save that value should it become relevant)
      unnecessary <- c("MEASURE", "MEASURE_label.en", "ACCOUNTING_ENTRY"
                       ,"ACCOUNTING_ENTRY_label.en","LEVEL_COUNTERPART","LEVEL_COUNTERPART_label.en"
                       ,"TIME_FORMAT","TIME_FORMAT_label.en", "UNIT", "UNIT_label.en", "POWERCODE", 
                       "POWERCODE_label.en", "OBS_STATUS", "OBS_STATUS_label.en")
      metadata <- unique(df[, c(unnecessary)])
      write.csv(metadata, file="1.2. FDI Income Metadata.csv", row.names = FALSE)
      
      #Drop variables
      df <- df[, !(names(df) %in% unnecessary)]
      rm(unnecessary) #remove list from memory
      
  # SPREAD DATA FROM LONG TO WIDE FORMAT
  # That is, format data so there is one observation for each country-pair in each year
  # Basically, this requires creating new columns for each possible type of flow, rather than having these in 
  # separate rows. 
    #Reformat using spread
    library(tidyr)
    df <- spread(df, "Flow", "obsValue")
    
      #Check that there is only one value for each country pair in each year
      check <- unique(df[, c("COU_label.en", "COUNTERPART_AREA_label.en", "obsTime")])
      #Check simply that 'check' and the dataframe has the same number of rows
      count(check, "COU")
      count(df, "COU") 
      rm(check)
  #TEMPORARY: Check various things
      #check <- subset(df, DI_T_D4P_F != 0)
      #check <- subset(df, DO_T_D4P_F != 0)
      
  #CREATE UNIQUE ID VARIABLE
    # Create id variable
    df$ID   <- paste(df$COU, df$COUNTERPART_AREA, df$obsTime, sep = "_" )
    df$Pair <- paste(df$COU, df$COUNTERPART_AREA, sep = "_" )
    
#DATA FIX.  
      #Some countries (e.g. Austria 2015) only report information for resident operation units (non-SPEs)
      #and the total is the confidential (because SPE data is confidential). 
      #In that case we would prefer to simply use the value for nonSPEs as the total value, as the SPE
      #value will be low enough to be confidential anyway. 
    
    vars <- c("FDIInc_Out_Total","FDIInc_Inw_Total","FDIInc_Out_Dividends","FDIInc_Inw_Dividends",
              "FDIInc_Out_Equity","FDIInc_Inw_Equity","FDIInc_Out_Debt", "FDIInc_Inw_Debt", 
              "FDIInc_Out_Reinvested", "FDIInc_Inw_Reinvested")
    
    #For each variable mentioned above, the loop checks if a value is missing from total, but not from NSPE. 
    #IF that is the case, it replaces the total with the NSPE. 
    for (x in vars) { # [[]] is the programmatic equivalent of $
      df[[x]][is.na(df[[x]]) & !is.na(df[[paste0(x, "_NSPE")]])] <- df[[paste0(x, "_NSPE")]][is.na(df[[x]]) & !is.na(df[[paste0(x, "_NSPE")]])]
    }
    rm(vars)
    rm(x)
    #Manual version
    #df$FDIInc_Out_Total[is.na(df$FDIInc_Out_Total) & !is.na(df$FDIInc_Out_Total_NSPE)] <- 
    #                    df$FDIInc_Out_Total_NSPE[is.na(df$FDIInc_Out_Total) & !is.na(df$FDIInc_Out_Total_NSPE)]
    
        #REORDER DATASET
    #We also drop the variables for SPE's and non-SPE's respectively, and focus only on totals
    df <- df[c("ID", "Pair", "COU","COU_label.en","COUNTERPART_AREA","COUNTERPART_AREA_label.en","obsTime",
               "FDIInc_Out_Total","FDIInc_Inw_Total","FDIInc_Out_Dividends","FDIInc_Inw_Dividends",
               "FDIInc_Out_Equity","FDIInc_Inw_Equity","FDIInc_Out_Debt", "FDIInc_Inw_Debt", 
               "FDIInc_Out_Reinvested", "FDIInc_Inw_Reinvested")]
    
    
    #SAVE WORLD DATA FOR EACH COU, THEN REMOVE THOSE AS ROWS
     noncountries <- c("WORLD", "WORLD unallocated and confidential")
  world <- df %>% subset(COUNTERPART_AREA_label.en %in% noncountries)
  df <- subset(df, !(COUNTERPART_AREA_label.en %in% noncountries)) #Remove the rows
  rm(noncountries) #remove
  world <- world[, colnames(world) %in% c("COU", "obsTime", "FDIInc_Out_Total", "COUNTERPART_AREA") ]
  world$COUNTERPART_AREA[world$COUNTERPART_AREA=="W0"]      <- "World_FDIInc_Out_Total"
  world$COUNTERPART_AREA[world$COUNTERPART_AREA=="C_W190"]  <- "Unallocated_FDIInc_Out_Total"
  
  
  
  #Spread data
  world <- spread(world, "COUNTERPART_AREA", "FDIInc_Out_Total")
  
  #Join back as columns in original data
  df <- left_join(df,world)
  rm(world)
  
  
  
  
  #SORT DATA AS PANEL DATA
  library(dplyr)
  df <- arrange(df, Pair, obsTime)


############################################################################        
#2. NATIONAL INCOME FROM OECD
############################################################################        
#After this step the dataset has been enriched with macroeconomic variables for the OECD countries. 
    
  #LOAD DATA
  #Load df from csv file
  library(readr)
  inc <- read_csv("2. National income (OECD).csv")
  
  #FILTER RELEVANT OBSERVATIONS
    #ONLY CURRENT PRICES, CURRENT EXCHANGE RATES
  inc <- subset(inc, inc$MEASURE=="CXC")
  
  #ONLY RELEVANT FLOWS
  keep <- c("B1_GS1", "D1_D4NFRS2", "D1_D4FRS2", "D1_D4TOS2", "B5_GS1", "K1MS1", "B5_NS1")  
  inc <- subset(inc, inc$TRANSACT %in% keep)
  rm(keep)

  #FILTER RELEVANT COLUMNS
  #THe dataset has both english and french labels. We drop the french ones. 
  inc <- inc[, -grep(pattern = ".fr", colnames(inc))] #Drops variables ending in .fr
  
  
  #Remove columns with more than one possible value for each country pair (except the main one)
  
    #Create a new flow variable with proper titles
    inc$Flow <- inc$TRANSACT
    inc$Flow[inc$Flow=="B1_GS1"]      <- "OECD_GDP"
    inc$Flow[inc$Flow=="D1_D4FRS2"]   <- "PrimInc_Receivable"
    inc$Flow[inc$Flow=="D1_D4TOS2"]   <- "PrimInc_Payable"
    inc$Flow[inc$Flow=="D1_D4NFRS2"]  <- "PrimInc_Net"
    inc$Flow[inc$Flow=="B5_GS1"]      <- "GNI"
    inc$Flow[inc$Flow=="K1MS1"]       <- "FixedCap"
    inc$Flow[inc$Flow=="B5_NS1"]      <- "NNI"
    
    #Save labels in separate dataset
    unnecessary <- c("TRANSACT", "TRANSACT_label.en")
    stroemlabels_inc <- unique(inc[, c("Flow", unnecessary)])
    write.csv(stroemlabels_inc, file="2.1. National Income Labels.csv", row.names = FALSE)
    library(dplyr)
    
    #Drop variables now saved in separate dataset
    inc <- inc[, !(names(inc) %in% unnecessary)]
    rm(unnecessary) #remove list from memory
  
  #Remove columns with only one possible value for all observations
  #Check that they only have one possible value (and save that value should it become relevant)
  unnecessary <- c("MEASURE", "MEASURE_label.en", "REFERENCEPERIOD", "REFERENCEPERIOD_label.en",
                   "TIME_FORMAT","TIME_FORMAT_label.en", "UNIT", "UNIT_label.en", "POWERCODE", 
                   "POWERCODE_label.en", "OBS_STATUS", "OBS_STATUS_label.en")
  metadata_inc <- unique(inc[, c(unnecessary)])
  write.csv(metadata_inc, file="2.2. National Income Metadata.csv", row.names = FALSE)
  
  #Drop variables
  inc <- inc[, !(names(inc) %in% unnecessary)]
  rm(unnecessary) #remove list from memory

  #SPREAD DATA FROM LONG TO WIDE FORMAT
  library(tidyr)
  inc <- spread(inc, "Flow", "obsValue")
  
  #REORDER COLUMNS
  inc <- inc[c("LOCATION", "LOCATION_label.en", "obsTime",    
               "OECD_GDP", "PrimInc_Net", "PrimInc_Receivable", "PrimInc_Payable", "GNI", "FixedCap", "NNI")]

  
  
  #MERGE DATA
  df <- left_join(df,inc, by = c("COU" = "LOCATION", "obsTime" = "obsTime", "COU_label.en" = "LOCATION_label.en"))
    rm(inc)
  
    #CREATE NEW VARIABLE: Directional principle: Outward - FDI income - Total AS SHARE OF Gross National Income
  #df$FDIInc_GNI <- df$DO_T_D4P_F / df$B5_GS1
          


  
############################################################################    
#3. TAX RATES FOR OECD COUNTRIES
###########################################################################    
#After this step the dataset will have been enriched with tax rates for all the OECD countries 
      #(i.e. the first part of each country pair)
      
  #LOAD DATA
  #Load df from csv file
  library(readr)
  tax <- read_csv("3. Corporate tax rates (OECD).csv")  
  
  #FILTER RELEVANT ROWS.
    #There are duplicates in COMB_CIT_RATE based on the variable TAR. 
    #Since we do not use the measure, we just drop them
    tax <- subset(tax, tax$CORP_TAX!="COMB_CIT_RATE")
  
  #FILTER RELEVANT COLUMNS
  #THe dataset has both english and french labels. We drop the french ones. 
  tax <- tax[, -grep(pattern = ".fr", colnames(tax))] #Drops variables ending in .fr
  
  #Save labels in separate dataset
  stroemlabels_tax <- unique(tax[, c("CORP_TAX", "CORP_TAX_label.en")])
  write.csv(stroemlabels_tax, file="3.1. Corporate tax rates Labels.csv", row.names = FALSE)
  library(dplyr)
  tax <- tax[, !(names(tax) %in% c("CORP_TAX_label.en"))]
  
  #Remove columns with only one possible value for all observations
  #Check that they only have one possible value (and save that value should it become relevant)
  unnecessary <- c("TAR", "TAR_label.en", "TIME_FORMAT", "TIME_FORMAT_label.en")
  metadata_tax <- unique(tax[, c(unnecessary)])
  write.csv(metadata_tax, file="3.2. Corporate tax rates Metadata.csv", row.names = FALSE)
  
  
    #Drop variables
    tax <- tax[, !(names(tax) %in% unnecessary)]
    rm(unnecessary)
  
  #CONVERT FROM LONG TO WIDE FORMAT
    library(tidyr)
    tax <- spread(tax, "CORP_TAX", "obsValue")  
    
  #MERGE THE DATASETS
  #We use a left join on country and year to merge in the various tax rate measures
  df <- left_join(df,tax)
  rm(tax) #drop tax data set


  

###########################################################################  
#4. GRAVITY DATA FROM CEPII
###########################################################################  
#After this step the dataset will have been enriched with GDP, distance and population
  
    #DOWNLOAD DATA
    #OBS: THIS DATASET IS NOT GENERATED BY APPENDIX ONE, BUT MUST BE DOWNLOADED MANUALLY. 
    #http://www.cepii.fr/CEPII/en/bdd_modele/download.asp?id=8
    #It is however available at the same github page as the others. 
  
    library(foreign)
    gravity <- read.dta("4. Gravity Data (CEPII).dta")
    #write.csv(gravity, file="gravdata.csv", row.names = FALSE) (large data-set, not the best idea)
    
    #REMOVE (SOME) IRRELEVANT ROWS
    #The main dataset only goes back to 2005. 
    gravity <- subset(gravity, year>=2005)
    
#DATA FIX
    #A number of countries (many havens, particularly dependecies) are not featured in the gravity dataset. 
    #We append a manually created dataset with data for (some of) these entitities
    library("readr")
    manual_gravdata <- read_csv("4.1. Gravity Data (Manual).csv", 
                                col_types = list(pop_o = col_number(), gdp_o = col_number(), gdp_d = col_number()
                                                 ,gdpcap_o = col_number(), gdpcap_d = col_number())) 

    
    #The manually added pairs above were only added for 2005. We therefore need to recreate the dataset for each year thereafter
    
    samler <- manual_gravdata #Temporary dataset
    
    for (i in 1:10) {
    temp <- manual_gravdata #Loop copies the dataset
    temp$year <- 2005 + i #changes the year
    samler <- rbind(samler, temp) #and saves the new year in the temporary dataset
    rm(temp)
    }
    rm(i)#The manually added pairs above were only added for 2005. We therefore need to carry the observations forward. 
    
    #Replace manual dataset with the updated one
    manual_gravdata <- samler
    rm(samler)
    
    #Append the data to the original dataset
    gravity <- rbind(gravity, manual_gravdata)
    rm(manual_gravdata)

#DATA FIX
    #FIX SOME ISSUES 
    gravity$iso3_d[gravity$iso3_d=="ZAR"] <- "COD" #Congo (Democratic Republic of the) has the wrong ISO-code
    gravity$iso3_d[gravity$iso3_d=="YUG"] <- "SRB" #We assign Serbia as Serbia and Montenegro in the dataset
    gravity$iso3_d[gravity$iso3_d=="TMP"] <- "TLS" #East Timor in gravity is called Timor-Leste in our dataset. 
    #This means we lose Montenegro from the dataset. Deemed acceptable. 
    
    #We add population where it is missing 
    #Niue source https://en.wikipedia.org/wiki/Niue
    gravity$pop_d[is.na(gravity$pop_d) &  gravity$iso3_d=="NIU" & gravity$year == 2005] <- 1612*10^-6

    #CREATE ID VARIABLE FOR MERGING
    gravity$ID <- paste(gravity$iso3_o, gravity$iso3_d, gravity$year, sep = "_" )    
    
    #REMOVE IRRELEVANT COLUMNS
    unnecessary <- c("iso3_o","iso3_d","year","iso2_o","iso2_d",
                     "comlang_ethno","col45", 
                    "tdiff","heg_o","heg_d","conflict","indepdate","col_to",
                    "area_o", "area_d",
                  "col_fr","curcol","sibling","cursib","empire","sever","sib_conflict",
                  "comcur","comrelig","comleg_pretrans","comleg_posttrans","transition_legalchange",
                  "legold_o","legold_d","legnew_o","legnew_d","gatt_o","gatt_d","pta_bb","fta_wto",
                  "fta_bb","fta_hmr","acp_to_eu","eu_to_acp","gsp_o_d","gsp_d_d","flaggsp_o_d",
                  "flaggsp_d_d","entry_cost_o","entry_cost_d","entry_proc_o","entry_proc_d","entry_time_o",
                  "entry_time_d","entry_tp_o","entry_tp_d","eu_o","eu_d")
    gravity <- gravity[, !(names(gravity) %in% unnecessary)]
    rm(unnecessary)
    
  #JOIN DATASETS
    df <- left_join(df,gravity, by = "ID")
    rm(gravity)
    
    
    #TEMPORARY: Distances dataset
    #http://www.cepii.fr/CEPII/en/bdd_modele/download.asp?id=6
    #library(foreign)
    #dist <- read.dta("dist_cepii.dta")
    library(dplyr)
    df <- rename(df, pop = pop_o )
    df <- rename(df, pop_j = pop_d )
    df <- rename(df, gdp = gdp_o )
    df <- mutate(df, gdp = gdp/10^6) #Rescale to millions
    df <- rename(df, gdp_j = gdp_d )
    df <- mutate(df, gdp_j = gdp_j/10^6) #Rescale to millions
    df <- rename(df, gdpcap = gdpcap_o )
    df <- rename(df, gdpcap_j = gdpcap_d )

#DATA FIX! 
    #There are quite a few missing population values. 
    #We assume, if no data is available, zero population growth 
    library(zoo)
    library(dplyr)
    
    #Population - Carries observations forward within each pair
    #na.locf.na <- function(x, na.rm = FALSE, ...) na.locf(x, na.rm = na.rm, ...)
    df <- arrange(df, COUNTERPART_AREA_label.en, obsTime)
    df <- df %>% group_by(COUNTERPART_AREA_label.en) %>% mutate(pop_j = na.locf(pop_j, na.rm = FALSE))
    df <- arrange(df, Pair, obsTime)
    
    #The manually added pairs above were only added for 2005. We therefore need to carry the observations forward. 
    

###########################################################################  
#5. GDP DATA FROM WORLD BANK
###########################################################################      
    #LOAD DATA
    #Load df from csv file
    library(readr)
    gdp <- read_csv("5. GDP (World Bank).csv")  

    
#DATA FIX
      #A lot of havens do not have gdp data in the world bank, particularly british dependecies. 
      #We append a manually created dataset with gdp data for (some of) these entitities
      manual_gdp <- read_csv("5.1. GDP (Manual).csv")  
      gdp <- rbind(gdp, manual_gdp)
      rm(manual_gdp)

#DATA FIX               
      #There are also some countries featured in the WB dataset, but with only missing values
      #We add some values for these. 
      #Gibraltar. Source: https://www.gibraltar.gov.gi/new/key-indicators
      gdp$value[is.na(gdp$value) & gdp$iso3c=="GIB" & gdp$date == 2011] <- 1201.31*10^6/0.646789
      gdp$value[is.na(gdp$value) & gdp$iso3c=="GIB" & gdp$date == 2012] <- 1317.06*10^6/0.633714
      gdp$value[is.na(gdp$value) & gdp$iso3c=="GIB" & gdp$date == 2013] <- 1484.28*10^6/0.607238
      gdp$value[is.na(gdp$value) & gdp$iso3c=="GIB" & gdp$date == 2014] <- 1637.75*10^6/0.640697
        
       #British virgin islands. Source http://data.un.org/CountryProfile.aspx?crName=British%20Virgin%20Islands
      gdp$value[is.na(gdp$value) & gdp$iso3c=="VGB" & gdp$date == 2005] <- 870*10^6
      gdp$value[is.na(gdp$value) & gdp$iso3c=="VGB" & gdp$date == 2010] <- 894*10^6
      gdp$value[is.na(gdp$value) & gdp$iso3c=="VGB" & gdp$date == 2014] <- 902*10^6
      
      #Curacao http://data.un.org/CountryProfile.aspx?crName=Cura%C3%A7ao
      gdp$value[is.na(gdp$value) & gdp$iso3c=="CUW" & gdp$date == 2005] <- 2345*10^6
      gdp$value[is.na(gdp$value) & gdp$iso3c=="CUW" & gdp$date == 2010] <- 2951*10^6
      gdp$value[is.na(gdp$value) & gdp$iso3c=="CUW" & gdp$date == 2014] <- 3159*10^6
      
      #Sint maarten source http://data.un.org/CountryProfile.aspx?crName=Sint%20Maarten%20(Dutch%20part)
      gdp$value[is.na(gdp$value) & gdp$iso3c=="SXM" & gdp$date == 2005] <- 705*10^6
      gdp$value[is.na(gdp$value) & gdp$iso3c=="SXM" & gdp$date == 2010] <- 892*10^6
      gdp$value[is.na(gdp$value) & gdp$iso3c=="SXM" & gdp$date == 2014] <- 1059*10^6
    
      #Turks and Caicos Islands http://data.un.org/CountryProfile.aspx?crName=Turks%20and%20Caicos%20Islands
      gdp$value[is.na(gdp$value) & gdp$iso3c=="TCA" & gdp$date == 2005] <- 579*10^6
      gdp$value[is.na(gdp$value) & gdp$iso3c=="TCA" & gdp$date == 2010] <- 687*10^6
      gdp$value[is.na(gdp$value) & gdp$iso3c=="TCA" & gdp$date == 2014] <- 797*10^6
      
      
    #FILTER RELEVANT COLUMNS
    gdp <- gdp[, colnames(gdp) %in% c("iso3c", "value", "date")]
    
    #GDP for OECD countries
    df <- left_join(df, gdp, by= c("COU" = "iso3c", "obsTime"="date"))
    df <- rename(df, GDP = value )
    df <- mutate(df, GDP = GDP/10^6) #Rescale to millions
    
    #GDP for partner countries
    df <- left_join(df, gdp, by= c("COUNTERPART_AREA" = "iso3c", "obsTime"="date"))
    df <- rename(df, GDP_j = value )
    df <- mutate(df, GDP_j = GDP_j/10^6) #Rescale to millions
    rm(gdp)
    
    #DATA FIX! 
    #There are quite a few missing GDP values for partner countries. 
    #We assume, if no data is available, zero gdp growth 
    library(zoo)
    library(dplyr)
    
    #GDP_j - Carries observations forward within each pair
    df <- arrange(df, COUNTERPART_AREA_label.en, obsTime)
    df <- df %>% group_by(COUNTERPART_AREA_label.en) %>% mutate(GDP_j = na.locf(GDP_j, na.rm = FALSE))
    #We also carry observations backwards. 
    df <- df %>% group_by(COUNTERPART_AREA_label.en) %>% mutate(GDP_j = na.locf(GDP_j, na.rm = FALSE, fromLast = TRUE))
    df <- arrange(df, Pair, obsTime)
    
    #DATA FIX. The gravity dataset has GDP values for Romania and Taiwan. We add those
    df$GDP_j[df$COUNTERPART_AREA == "TWN"] <- df$gdp_j[df$COUNTERPART_AREA == "TWN"] #Chinese Taipei
    df$GDP_j[df$COUNTERPART_AREA == "ROM"] <- df$gdp_j[df$COUNTERPART_AREA == "ROM"] #Romania
  
############################################################################     
#6. LIST OF TAX HAVENS
############################################################################
    #LOAD DATA
    #Load df from csv file
    library(readr)
    havens <- read_csv("6. Tax Havens (Manual).csv")   
    
    df <- left_join(df,havens, by = c("COUNTERPART_AREA" = "ISO"))
    rm(havens) #Delete temporary dataset
    
    check <- unique(df[, c("COUNTERPART_AREA_label.en", "Haven", "All_havens")])
    check <- na.omit(check)
    rm(check)
    #OBS: THERE IS NO DATA FOR MONACO IN THE OECD DATASET. 
    
    #Fill in 0's for non-havens
    df$Haven[is.na(df$Haven)] <- 0
    df$All_havens[is.na(df$All_havens)] <- 0
    df$Small_Haven[is.na(df$Small_Haven)] <- 0
    df$Large_Haven[is.na(df$Large_Haven)] <- 0
    
    
    #The automatic approach had problems, as countries shifted across the dividng line
    #I added the variables to the manual list, based on rule below applied on averages.
    #Create new Haven variables based on population
      #df$Small_Haven1 <- df$Haven
      #df$Small_Haven1[df$pop_j>0.4] <- 0
    
    #Create new Haven variables based on population
      #df$Large_Haven1 <- df$Haven
      #df$Large_Haven1[df$pop_j<0.4] <- 0
     
    
    
############################################################################    
#7. EXCHANGE RATES FOR OECD COUNTRIES
############################################################################    
#After this step the dataset will have been enriched with exchange rates for all the OECD countries 
#(i.e. the first part of each country pair)

    #LOAD DATA
    #Load df from csv file
    library(readr)
    exc <- read_csv("7. Exchange rates (OECD).csv")   
    head(exc)
    
    #FILTER RELEVANT OBSERVATIONS
    #ONLY RELEVANT FLOWS
    keep <- c("EXC","EXCE")  
    exc <- subset(exc, exc$TRANSACT %in% keep)
    rm(keep)
    
    #FILTER RELEVANT COLUMNS
    #THe dataset has both english and french labels. We drop the french ones. 
    exc <- exc[, -grep(pattern = ".fr", colnames(exc))] #Drops variables ending in .fr
    
    #Remove columns with more than one possible value for each country pair (except the main one)
    #Save labels in separate dataset
    unnecessary <- c("TRANSACT_label.en" )
    stroemlabels_exc <- unique(exc[, c("TRANSACT", unnecessary)])
    write.csv(stroemlabels_exc, file="7.1. Exchange rates Labels.csv", row.names = FALSE)
    
    #Drop variables now saved in separate dataset
    exc <- exc[, !(names(exc) %in% unnecessary)]
    rm(unnecessary) #remove list from memory
    
    #Remove columns with only one possible value for all observations
    #Check that they only have one possible value (and save that value should it become relevant)
    unnecessary <- c("MEASURE", "MEASURE_label.en", 
                     "TIME_FORMAT","TIME_FORMAT_label.en", "UNIT", "UNIT_label.en", "POWERCODE", 
                     "POWERCODE_label.en")
    metadata_exc <- unique(exc[, c(unnecessary)])
    write.csv(metadata_exc, file="7.2. Exchange rates Metadata.csv", row.names = FALSE)
    
    #Drop variables
    exc <- exc[, !(names(exc) %in% unnecessary)]
    rm(unnecessary) #remove list from memory
    
    #SPREAD DATA FROM LONG TO WIDE FORMAT
    library(tidyr)
    exc <- spread(exc, "TRANSACT", "obsValue")
    
    #MERGE DATA
    df <- left_join(df,exc, by = c("COU" = "LOCATION", "obsTime" = "obsTime", "COU_label.en" = "LOCATION_label.en"))
    rm(exc)
    
    
############################################################################    
#8. CFC Rules Data
############################################################################

    library("readr")
    cfc <- read.csv("8. CFC Rules.csv")
    
    df <- left_join(df, cfc, by =c("COU"="ISO"))
    rm(cfc)
    check <- unique(df[, c("COU_label.en", "CFC")]) %>% subset(CFC==1)
    rm(check)

############################################################################    
#9. TAX RATES FROM KPMG
############################################################################

library("readr")
kpmg <- read.csv("9. Corporate tax rates (KPMG).csv")

#DATA FIX. 
    #There is no data on 2005. We assume no change from 2005 to 2006. 
    kpmg$X2005 <- kpmg$X2006
    
    #We 
    kpmg <- gather(kpmg, Aar, CIT_RATE_KPMG, X2006:X2005)
    #We remove X (first letter) from the years. 
    kpmg$Aar <- substring(kpmg$Aar,2)
    
    #Convert to proper variable types
    kpmg$Aar <- as.integer(kpmg$Aar)
    kpmg$ISO <- as.character(kpmg$ISO)
    
    #Join data for both countries
    #Reporting countries (for checking purposes)
    df <- left_join(df,kpmg, by = c("COU"="ISO", "obsTime" = "Aar"))
    
    #Partner countries (most relevant)
    kpmg <- rename(kpmg, CIT_RATE_KPMG_j = CIT_RATE_KPMG )
    df <- left_join(df,kpmg, by = c("COUNTERPART_AREA"="ISO", "obsTime" = "Aar"))
    rm(kpmg)
    
    #Create variable for difference
    df$CIT_difference <- df$CIT_RATE - df$CIT_RATE_KPMG_j
    
    
###########################################################################  
#10. FINAL CLEANING OF DATA & EXPORT
###########################################################################  

#DATA FIX 
    #The OECD data features zeros from country to itself (e.g. Denmark to Denmark).
    #These should be removed before modelling 
    df <- subset(df, COU != COUNTERPART_AREA)
      
#DATA FIX
    #Some countries report only aggregates (or not even that)
      #Finland
      check <- subset(df, df$COU_label.en == "Finland")
      rm(check)
      df <- subset(df, df$COU_label.en != "Finland")
      
      #Luxembourg
      check <- subset(df, df$COU_label.en == "Luxembourg")
      rm(check)
      df <- subset(df, df$COU_label.en != "Luxembourg")
      
      #Mexico (already missing)

      #Portugal
      check <- subset(df, df$COU_label.en == "Portugal")
      rm(check)
      df <- subset(df, df$COU_label.en != "Portugal")
      
      #Portugal
      check <- subset(df, df$COU_label.en == "Portugal")
      rm(check)
      df <- subset(df, df$COU_label.en != "Portugal")
      
      #Switzerland
      check <- subset(df, df$COU_label.en == "Switzerland")
      rm(check)
      df <- subset(df, df$COU_label.en != "Switzerland")
      
      #Turkey
      check <- subset(df, df$COU_label.en == "Turkey")
      rm(check)
      df <- subset(df, df$COU_label.en != "Turkey")
      
      
#EXPORT DATA FOR LATER USE
      write.csv(df, file="10. Main Dataset.csv", row.names = FALSE)
      write.dta(df, file="10. Main Dataset.dta")
      
    
###########################################################################  
#END
###########################################################################  

    