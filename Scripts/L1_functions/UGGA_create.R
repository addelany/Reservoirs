## AUTOMATED L1 QAQC SCRIPT FOR UGGA 
#This QAQC cleaning script was applied to create the data files included in this data package
## Authors: Austin Delany and Abby Lewis
## Last edited: 2026-11-19 A. Breef-Pilz did major updates and added in a maintenance log, combine historical files, and subset data
## 12 Feb 2025- added a statement to end the script if no new data for the year. 
## 16 Jan 2026 - moved reps down to be calculated after the maintenance log
## 19 Nov 2026 - used this script as a test for Dat

## Additional notes: This script is included with this EDI package to show which QAQC has already been applied to generate these data. This script is only for internal use by the data creator team and is provided as a reference; it may not run as-is.

ugga_qaqc <- function(files, 
                      maintenance_file, 
                      outfile,
                      start_date, 
                      end_date){
  
  # files <- "./Data/DataNotYetUploadedToEDI/UGGA/"
  # maintenance_file <- "./Data/DataNotYetUploadedToEDI/UGGA/UGGA_Maintenance_Log.csv"
  # outfile <- "test_UGGA.csv"
  # start_date <- NULL
  # end_date <- NULL
  
  # add in historic files for EDI
 
    # read in files
    
    UGGA_files <- list.files(path=files, pattern="season_Flux_Output.csv", full.names=TRUE, recursive = TRUE)
    
    # take out the 2017 files 
    UGGA_files <- UGGA_files[ !grepl("2017", UGGA_files) ]

    
    # read in the files and bind them together
  
    all<-UGGA_files%>%
      map_df(~ read_csv(.x, show_col_types = FALSE))|>
      # put the Start_time and End_time in all the same column
      mutate(Start_time = dplyr::coalesce(Start, Start_time),
             End_time = dplyr::coalesce(End, End_time))|>
      # # rewrite the rep numbers to make sure they are grouped by Reservoir, Site, and Date
      # # move down lower after things removed from maintenance log
      # group_by(Reservoir, Site, Date)%>%
      # mutate(Rep = seq(1:n()))%>%
      # ungroup()%>%
      select(!c(Start, End))
    
    
    ### Read in the maintenance log
    ## read in maintenance file 
    log <- read_csv(maintenance_file, col_types = cols(
      .default = col_character(),
      TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
      TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
      flag = col_integer()
    ))
    
    # Set timezone as America/New_York. 
    log$TIMESTAMP_start <- force_tz(as.POSIXct(log$TIMESTAMP_start), tzone = "America/New_York")
    log$TIMESTAMP_end <- force_tz(as.POSIXct(log$TIMESTAMP_end), tzone = "America/New_York")
    
    ### identify the date subsetting for the data
    if (!is.null(start_date)){
      all <- all %>% 
        filter(Date >= start_date)
      log <- log %>% 
        filter(TIMESTAMP_start <= end_date)
    }
    
    if(!is.null(end_date)){
      all <- all %>% 
        filter(Date <= end_date)
      log <- log %>% 
        filter(TIMESTAMP_end >= start_date)
      
    }
    
    # Check if there are any files for the L1. If not then end the script
    
    if(nrow(all)==0){
      
      print("No new files for the current year")
      
    }else{
  
  # Reorder the data frame

  ugga_current = all%>%
    mutate(Date=as.Date(Date))|>
           #Start=paste0(Start),
           #End = paste0(End))%>%
    rename(Flag_co2_flux_umolCm2s = co2_flux_umolCm2s_flag,
           Flag_ch4_flux_umolCm2s = ch4_flux_umolCm2s_flag)%>%
    # make a datetime column for maintenance
    mutate(maint_StartTime = force_tz(as.POSIXct(paste0(Date, Start_time)), tzone = "America/New_York"),
           maint_EndTime = force_tz(as.POSIXct(paste0(Date, End_time)), tzone = "America/New_York"))%>%
    # add this to the maintenance log
    #filter(!Date == "2022-06-13"|!(Start_time %in% c("11:14:08","11:22:16","11:30:14")))%>%#only one real sampling on these three takes, removing issues caused by data processing
    #rename(Start_time = Start,
          # End_time = End) |> 
    dplyr::select("Reservoir",
                  "Site",
                  "Date",
                  "maint_StartTime",
                  "maint_EndTime",
                  "Rep",
                  "Start_time",
                  "End_time",
                  "Temp_C",
                  "co2_slope_ppmS",
                  "co2_R2",
                  "co2_flux_umolCm2s",
                  "ch4_slope_ppmS",
                  "ch4_R2",
                  "ch4_flux_umolCm2s",
                  "Flag_co2_flux_umolCm2s",
                  "Flag_ch4_flux_umolCm2s") %>%
    arrange(Date)
  
  ## add in the maintenance log 
  
  
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
      
      
      
      ### Get the Maintenance Flag
      
      flag <- log$flag[i]
      
      
      ### Get the new value for a column or an offset
      
      update_value <- as.numeric(log$update_value[i])
      
      
      
      ### Get the names of the columns affected by maintenance
      
      colname_start <- log$start_parameter[i]
      
      colname_end <- log$end_parameter[i]
      
      
      
      ### if it is only one parameter parameter then only one column will be selected
      
      
      
      if(is.na(colname_start)){
        
        
        
        maintenance_cols <- colnames(ugga_current%>%select(colname_end))
        
        
        
      }else if(is.na(colname_end)){
        
        
        
        maintenance_cols <- colnames(ugga_current%>%select(colname_start))
        
        
        
      }else{
        
        maintenance_cols <- colnames(ugga_current%>%select(colname_start:colname_end))
        
      }
      
      
      
      if(is.na(end)){
        
        # If there the maintenance is on going then the columns will be removed until
        
        # and end date is added
        
        Time <- ugga_current |> filter(maint_StartTime >= start) |> select(maint_StartTime)
        
      }else if (is.na(start)){
        
        # If there is only an end date change columns from beginning of data frame until end date
        
        Time <- ugga_current |> filter(maint_StartTime <= end) |> select(maint_StartTime)
        
      }else {
        
        Time <- ugga_current |> filter(maint_StartTime >= start & maint_StartTime <= end) |> select(maint_StartTime)
        
      }
      
      ### This is where information in the maintenance log gets updated
      
      
      
      if(flag %in% c(2) ){ ## UPDATE THIS WITH ANY NEW FLAGS
        
        # remove observations observations 
        
        ugga_current[ugga_current$maint_StartTime %in% Time$maint_StartTime &
                       
                       ugga_current$Site %in% c(Site_temp) & 
                          
                          ugga_current$Reservoir %in% c(Reservoir), maintenance_cols] <- NA
        
        # flag the the observations
        ugga_current[ugga_current$maint_StartTime %in% Time$maint_StartTime &
                       
                       ugga_current$Site %in% c(Site_temp) & 
                          
                          ugga_current$Reservoir %in% c(Reservoir), paste0("Flag_",maintenance_cols)] <- flag
        
        
        
      }else if (flag %in% c(3)){
        
        # value is suspect
        
        ugga_current[ugga_current$maint_StartTime %in% Time$maint_StartTime &
                          
                          ugga_current$Site %in% c(Site_temp) & 
                          
                          ugga_current$Reservoir %in% c(Reservoir), paste0("Flag_",maintenance_cols)] <- flag
        
        
      }else if (flag %in% c(100)){
        
        # to be deleted
        
        ugga_current <- ugga_current[!(ugga_current$maint_StartTime %in% Time$maint_StartTime & 
                                         ugga_current$Reservoir %in% Reservoir &
                                                            ugga_current$Site %in% Site_temp),]
        
      }else{
        
        warning("Flag not coded in the L1 script. See Austin or Adrienne")
        
      }
      
    } # end for loop
    
  } #end conditional statement
  

# #### END MAINTENANCE LOG CODE #####
  
  # Add some additional flags

  ugga_flagged <- ugga_current %>%
    # take out the maintenance log columns
    select(!c(maint_StartTime, maint_EndTime))%>%
    # flag if the r2 is less than 0.9
    mutate(Flag_co2_flux_umolCm2s = ifelse(co2_R2<0.9,1,Flag_co2_flux_umolCm2s),
           Flag_ch4_flux_umolCm2s = ifelse(ch4_R2<0.9,1,Flag_ch4_flux_umolCm2s),
           
      # change the site from 1 to 40 in BVR
           Site = ifelse(year(Date)==2022 & Site==1 & Reservoir=="BVR",40,Site))%>%
    rename(CO2Slope_ppmS = co2_slope_ppmS,
           CH4Slope_ppmS = ch4_slope_ppmS,
           CO2_R2 = co2_R2,
           CH4_R2 = ch4_R2,
           CH4Flux_umolCm2s = ch4_flux_umolCm2s,
           CO2Flux_umolCm2s = co2_flux_umolCm2s,
           Flag_CO2Flux_umolCm2s = Flag_co2_flux_umolCm2s,
           Flag_CH4Flux_umolCm2s = Flag_ch4_flux_umolCm2s)%>%
    
    # set the time for the 2022 to noon because time is wrong. 
    mutate(End_dif = as.POSIXct(End_time, format = "%H:%M:%S")-as.POSIXct(Start_time, format = "%H:%M:%S"),
           Start_time = ifelse(year(Date)==2022,"12:00:00",format(Start_time, format = "%H:%M:%S")),
           End_time = ifelse(year(Date)==2022,format(as.POSIXct("12:00:00", format = "%H:%M:%S")+End_dif,"%H:%M:%S"),format(End_time, format = "%H:%M:%S")),
           # if the start time is exactly noon then it is set to that time and should be flagged
           Flag_Start_time = ifelse(Start_time == "12:00:00",1,0),
           Flag_End_time = ifelse(Start_time == "12:00:00",1,0))|>
    select(!End_dif)|>
    group_by(Reservoir, Site, Date)%>%
    mutate(Rep = seq(1:n()))%>%
    ungroup()
  
  # drop inflow and saw grass sites
  final <- ugga_flagged |> 
    mutate(Site = as.numeric(Site), 
           Start_time = as.character(Start_time), 
           End_time = as.character(End_time)) 
  
  
  
  ### 7. Save the file. If outfile is NULL then return the file
  
  if(is.null(outfile)){
    
    return(final)
  }else{
    
    final$Date <- as.character(format(final$Date)) # convert DateTime to character
    
    write_csv(final, outfile)
    }
  }
}


### These are my changes
