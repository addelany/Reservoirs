# ysi_qaqc_2023.R
# QAQC of YSI and PAR data from 2023
# Created by ADD, modified by HLW, and ABP
# First developed: 2023-12-04
# Last edited: 2026-11-19 
# changed as.Date to as.Date.character because as.Date would add a day if time close to mindnight
# 2025-02-18 added an if statement to end the function if there were no observations for the year
# 2025-11-12 added a print statement for the dups and condensed some of the file type renaming 

#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate, dplyr,
                gsheet) ## Use pacman package to install/load other packages


ysi_qaqc <- function(data_file, 
                     gsheet_data = TRUE, 
                     maintenance_file = NULL, 
                     outfile,
                    start_date = NULL,
                    end_date = NULL){
  

  if(is.character(data_file) & gsheet_data == FALSE){
    # read catwalk data and maintenance log
    # NOTE: date-times throughout this script are processed as UTC
    raw_profiles <- read_csv(data_file, skip = 1, col_names = CATPRES_COL_NAMES,
                          col_types = cols(.default = col_double(), DateTime = col_datetime()))

  } else if (is.character(data_file) & gsheet_data == TRUE){
    raw_profiles <- gsheet::gsheet2tbl(data_file)

  }else {
    raw_profiles <- data_file
  }


# gsheet_url <- 'https://docs.google.com/spreadsheets/d/1HbSBEFjMuK4Lxit5MRbATeiyljVAB-cpUNxO3dKd8V8/edit#gid=1787819257'
# raw_profiles <- gsheet::gsheet2tbl(gsheet_url)

#date format. Check warnings about parsing and figure out the issue. 
raw_profiles$DateTime = lubridate::parse_date_time(raw_profiles$DateTime, orders = c('ymd HMS','ymd HM','ymd', 'mdy','mdy HM', 'mdy HMS', 'mdy HM'), tz = "America/New_York")


# remove notes column
raw_profiles$Notes <- NULL


  ### identify the date subsetting for the data
  if (!is.null(start_date)){
    #force tz check 
    start_date <- force_tz(as.POSIXct(start_date), tzone = "America/New_York")
    
    raw_profiles <- raw_profiles %>% 
      filter(DateTime >= start_date)
  }
  
  if(!is.null(end_date)){
    #force tz check 
    end_date <- force_tz(as.POSIXct(end_date), tzone = "America/New_York")
    
    raw_profiles <- raw_profiles %>% 
      filter(DateTime <= end_date)
  }
 
#  # Check if there are any files for the L1. If not then end the script

if(nrow( raw_profiles)==0){
  
  print("No new files for the current year")
  
}else{

## AUTOMATED FLAGS THAT CAN BE APPLIED TO ENTIRE TABLE BY INDEX ##
for(j in colnames(raw_profiles%>%select(DateTime,Temp_C:pH))) { 
  
  #create new flag column in data frame and set to zero
  raw_profiles[,paste0("Flag_",j)] <- 0 #creates flag column + name of variable
  
  #puts in flag 1 if value not collected
  raw_profiles[c(which(is.na(raw_profiles[,j]))),paste0("Flag_",j)] <- 1
}


# ### Create a DateTime Flag for non-recorded times ####
# # (i.e., 12:00) and set to noon
# # Convert time that are in 12 hours to 24 hours
raw_profiles <- raw_profiles %>%
  mutate(Time = format(DateTime,"%H:%M:%S"),
         #Time = ifelse(Time == "00:00:00", "12:00:00",Time),
         Flag_DateTime = ifelse(Time == "12:00:00", 1, Flag_DateTime), # Flag if set time to noon
         #just need to catch some of the old data that didn't properly get flagged
         Date = as.Date.character(DateTime),
         DateTime = ymd_hms(paste0(Date, "", Time), tz = "America/New_York"))|>
         # Hours = hour(DateTime),
         # DateTime = ifelse(Hours<5, DateTime + (12*60*60), DateTime), # convert time to 24 hour time
         # DateTime = as_datetime(DateTime, tz = "America/New_York"))%>% # time is in seconds put it in ymd_hms
  select(-c(Time, Date))

#make sure other columns are the correct type and update raw data into new table to make rerunnig easier

update_profiles <- raw_profiles |>
  mutate(Reservoir = as.character(Reservoir),
         across(c(Site, Depth_m:pH), as.numeric))


## ADD FLAGS (note that only flags 2, 5, and 6 can come from the maintenance file)
# 0 - NOT SUSPECT
# 1 - SAMPLE NOT TAKEN
# 2 - INSTRUMENT MALFUNCTION
# 3 - SAMPLE BELOW DETECTION LIMIT
# 4 - NEGATIVE VALUE SET TO ZERO
# 5 - SUSPECT SAMPLE
# 6 - HUMAN ERROR
# 7 - TEMP MEASURED USING PH PROBE


if (!is.null(maintenance_file)){ # check to see if maint log is non-null value
  
  ## ADD MAINTENANCE LOG FLAGS (manual edits to the data for suspect samples or human error)
  #maintenance_file <- 'Data/DataNotYetUploadedToEDI/YSI_PAR/maintenance_log.csv'
  log_read <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
  
  log <- log_read

  # subset the maintenance log if there are defined start and end times
  if (!is.null(start_date)){
    
    log <- log %>% 
      filter(TIMESTAMP_start <= end_date)
  }
  
  if(!is.null(end_date)){
    
    log <- log %>% 
      filter(TIMESTAMP_end >= start_date)
  }
  
  ## filter maintenance log is there are star
  if(nrow(log)==0){
    print('No Maintenance Events Found...')
    
  } else {
  
  for(i in 1:nrow(log)){
    ### Assign variables based on lines in the maintenance log.
    
    ### get start and end time of one maintenance event
    start <- force_tz(as.POSIXct(log$TIMESTAMP_start[i]), tzone = "America/New_York")
    end <- force_tz(as.POSIXct(log$TIMESTAMP_end[i]), tzone = "America/New_York")
    
    ### Get the Reservoir Name
    Reservoir <- log$Reservoir[i]
    
    ### Get the Site Number
    Site_temp <- as.numeric(log$Site[i])
    
    ### Get the depth value
    Depth <- as.numeric(log$Depth[i]) 
    
    ### Get the Maintenance Flag
    flag <- log$flag[i]
    
    ### Get the new value for a column or an offset
    update_value <- as.numeric(log$update_value[i])
    
    ### Get the names of the columns affected by maintenance
    colname_start <- log$start_parameter[i]
    colname_end <- log$end_parameter[i]
    
    ### if it is only one parameter parameter then only one column will be selected
    
    if(is.na(colname_start)){
      
      maintenance_cols <- colnames(update_profiles%>%select(colname_end))
      
    }else if(is.na(colname_end)){
      
      maintenance_cols <- colnames(update_profiles%>%select(colname_start))
      
    }else{
      maintenance_cols <- colnames(update_profiles%>%select(colname_start:colname_end))
    }
    
    if(is.na(end)){
      # If there the maintenance is on going then the columns will be removed until
      # and end date is added
      Time <- update_profiles |> filter(DateTime >= start) |> select(DateTime)
      
    }else if (is.na(start)){
      # If there is only an end date change columns from beginning of data frame until end date
      Time <- update_profiles |> filter(DateTime <= end) |> select(DateTime)
      
    }else {
      Time <- update_profiles |> filter(DateTime >= start & DateTime <= end) |> select(DateTime)
    }
    
    ### This is where information in the maintenance log gets updated
    
    if(flag %in% c(1,2)){
      # The observations are changed to NA for maintenance or other issues found in the maintenance log
      update_profiles[update_profiles$DateTime %in% Time$DateTime &
                        update_profiles$Reservoir %in% c(Reservoir) &
                        update_profiles$Site %in% c(Site_temp) & 
                        update_profiles$Depth_m %in% c(Depth), maintenance_cols] <- NA
      update_profiles[update_profiles$DateTime %in% Time$DateTime &
                        update_profiles$Reservoir %in% c(Reservoir) &
                        update_profiles$Site %in% c(Site_temp) & 
                        update_profiles$Depth_m %in% c(Depth), paste0("Flag_",maintenance_cols)] <- flag
      
    }else if (flag %in% c(3)){
      ## BDL
      update_profiles[update_profiles$DateTime %in% Time$DateTime &
                        update_profiles$Reservoir %in% c(Reservoir) &
                        update_profiles$Site %in% c(Site_temp) & 
                        update_profiles$Depth_m %in% c(Depth), maintenance_cols] <- NA
      update_profiles[update_profiles$DateTime %in% Time$DateTime &
                        update_profiles$Reservoir %in% c(Reservoir) &
                        update_profiles$Site %in% c(Site_temp) & 
                        update_profiles$Depth_m %in% c(Depth), paste0("Flag_",maintenance_cols)] <- flag
      
    }else if (flag %in% c(4)){
      ## change negative values are changed to 0
      
      update_profiles[update_profiles$DateTime %in% Time$DateTime &
                        update_profiles$Reservoir %in% c(Reservoir) &
                        update_profiles$Site %in% c(Site_temp) & 
                        update_profiles$Depth_m %in% c(Depth), maintenance_cols] <- 0
      update_profiles[update_profiles$DateTime %in% Time$DateTime &
                        update_profiles$Reservoir %in% c(Reservoir) &
                        update_profiles$Site %in% c(Site_temp) & 
                        update_profiles$Depth_m %in% c(Depth), paste0("Flag_",maintenance_cols)] <- flag
      
    } else if(flag %in% c(5)){
      # Suspect sample
      update_profiles[update_profiles$DateTime %in% Time$DateTime &
                        update_profiles$Reservoir %in% c(Reservoir) &
                        update_profiles$Site %in% c(Site_temp) & 
                        update_profiles$Depth_m %in% c(Depth), paste0("Flag_",maintenance_cols)] <- flag
      
    }else if(flag %in% c(6) & (colname_start != 'Site' | colname_start != 'Depth_m')){
      ## human error
      update_profiles[update_profiles$DateTime %in% Time$DateTime &
                        update_profiles$Reservoir %in% c(Reservoir) &
                        update_profiles$Site %in% c(Site_temp) & 
                        update_profiles$Depth_m %in% c(Depth), maintenance_cols] <- update_value
      update_profiles[update_profiles$DateTime %in% Time$DateTime &
                        update_profiles$Reservoir %in% c(Reservoir) &
                        update_profiles$Site %in% c(Site_temp) & 
                        update_profiles$Depth_m %in% c(Depth), paste0("Flag_",maintenance_cols)] <- flag
      
    }else if(flag %in% c(6) & (colname_start == 'Site' | colname_start == 'Depth_m')){
      print(start)
      print(update_value)
      ## human error for site, which we don't indicate in final dataset
      update_profiles[update_profiles$DateTime %in% Time$DateTime &
                        update_profiles$Reservoir %in% c(Reservoir) &
                        update_profiles$Site %in% c(Site_temp) & 
                        update_profiles$Depth_m %in% c(Depth), maintenance_cols] <- update_value
      
    }else{
      warning("Flag not coded in the L1 script. See Austin or Adrienne")
      }
    }#end for loop
  }#end conditional statement
}

#### END MAINTENANCE LOG CODE

# ## check for values less than or equal to 0 to flag as 4 and change to 0 
for(j in colnames(update_profiles%>%select(DO_mgL:SpCond_uScm, pH))){ # omit temp and ORP because it can be negative
  ## set flags for negative values
  update_profiles[c(which(update_profiles[,j] <= 0)),paste0("Flag_",j)] <- 4
  update_profiles[c(which(update_profiles[,j] <= 0)),j] <- 0 #replaces value with 0
}

## AUTOMATED FLAGS THAT ARE DONE FOR EACH VARIABLE INDIVIDUALLY (Instrument malfunction / MDL)
# Supected values and instrument malfunction
update_profiles <- update_profiles |>
  mutate(
    # Flags for pH 
        Flag_pH = ifelse((!is.na(pH) & (pH > 14 | pH < 4)), 2, Flag_pH),
         # Between 4 and 5 is suspect sample
         Flag_pH = ifelse((!is.na(pH) & pH>4 & pH<5), 5, Flag_pH), 
        
        # Flags for ORP
         #looking at the manual the range for ORP is -1999 - 1999
         Flag_ORP_mV = ifelse((!is.na(ORP_mV) & (ORP_mV < -1999 | ORP_mV >= 1999)), 2, Flag_ORP_mV),
        # Suspect samples between 750 and 1999
        Flag_ORP_mV = ifelse((!is.na(ORP_mV) & ORP_mV > 750 & Flag_ORP_mV !=2), 5, Flag_ORP_mV),
        
         #Flag_PAR_umolm2s = No bounds given,
        
        # Flag for Temperature
         Flag_Temp_C = ifelse((!is.na(Temp_C) & (Temp_C >= 40)), 2, 
                              ifelse(!is.na(Temp_C) & !is.na(pH) & is.na(DO_mgL) & is.na(Cond_uScm), 7,
                              Flag_Temp_C)), # 7 flag for temp measured with ph probe
        # Temp suspect from 35- 40 
        Flag_Temp_C = ifelse((!is.na(Temp_C) & Temp_C>35 & Temp_C<40), 2, Flag_Temp_C),
        
         # DO Range is 0- 50mgL for sensor
         Flag_DO_mgL = ifelse((!is.na(DO_mgL) & (DO_mgL > 50)), 2, Flag_DO_mgL),
        
         # DO Range is 0 - 500% for sensor
         Flag_DOsat_percent = ifelse((!is.na(DOsat_percent) & (DOsat_percent > 200)), 2,Flag_DOsat_percent),
        
         # Cond Range 0 - 200000 for sensor
         Flag_Cond_uScm = ifelse((!is.na(Cond_uScm) & ((Cond_uScm < 10 | Cond_uScm > 1000))), 2, Flag_Cond_uScm),
        
         # Cond Range 0 - 200000 for sensor but for 
         Flag_SpCond_uScm = ifelse((!is.na(SpCond_uScm) & (SpCond_uScm > 1000)), 2, Flag_SpCond_uScm)
  )


# Change observations to NA that are flagged as 2
for(j in colnames(update_profiles%>%select(DO_mgL:PAR_umolm2s, pH))){ 
  ## set values to NA fpr Flag 2
  update_profiles[c(which(update_profiles[,paste0("Flag_",j)] == 2)),j] <- NA #replaces value with NA
}



## ORGANIZE FINAL TABLE ##
ysi <- update_profiles |>
  select(Reservoir, Site, DateTime, Depth_m, Temp_C, DO_mgL, DOsat_percent,
         Cond_uScm, SpCond_uScm, PAR_umolm2s, ORP_mV, pH, Flag_DateTime, Flag_Temp_C, Flag_DO_mgL, Flag_DOsat_percent,
         Flag_Cond_uScm, Flag_SpCond_uScm, Flag_PAR_umolm2s, Flag_ORP_mV, Flag_pH) |>
  arrange(Reservoir, DateTime, Depth_m)



## CHECK FOR DUPLICATES
dup_check <- ysi |>
  group_by(Reservoir, Site, DateTime, Depth_m) |>
  mutate(n = n()) |>
  filter(n > 1)

if (nrow(dup_check) > 0){
  warning('DUPLICATE DATA FOUND - removing duplicates')
  
  #print the duplicated rows
  print(dup_check)
  
  duplicates_df <- ysi

  # remove duplicated rows
  deduped_df <- duplicates_df |>
    distinct(Reservoir, Site, DateTime,Depth_m, .keep_all = TRUE)

  ysi <- deduped_df
}


if (!is.null(outfile)){
# Write to CSV -- save as L1 file

  # set date to characer when saving it
  ysi$DateTime <- as.character(format(ysi$DateTime))
  
write_csv(ysi, outfile)
}

return(ysi)
  } # ends if statement
} # ends function

# maintenance_file <- 'Data/DataNotYetUploadedToEDI/YSI_PAR/maintenance_log.csv'
# data_file <- 'https://docs.google.com/spreadsheets/d/1MX__IelyQBHO1bNxAltfYT_r_pJisMuiMtDG4oxkOok/'
# outfile <- 'Data/DataNotYetUploadedToEDI/YSI_PAR/ysi_L1.csv'
# 
# ysi_qaqc(data_file = data_file,
#         maintenance_file = maintenance_file,
#         gsheet_data = TRUE,
#         outfile = outfile)
