#------------------------
#Setup Dynamic TOPMODEL observations
#------------------------
## presume running in root of github repository
rm(list=ls())

wd <- "./dynatop"

##Find all necessary files with climate (NLDAS) and discharge (NWIS) data
precip_files = list.files(wd,pattern="APCP*.+.txt", full.names=TRUE)
pet_files = list.files(wd,pattern="PEVAP*.+.txt", full.names=TRUE)
flow_file = list.files(wd,pattern="NWIS*.+.txt",full.names=TRUE)
  
#Pre-allocate memory for data records (NOTE: set nrows equal to the number of records being imported; 1 year = 8759 hourly records in 2010)
precip = matrix(NA,ncol = length(precip_files),nrow = 8759)
pet = matrix(NA,ncol = length(precip_files),nrow = 8759)

#Load NLDAS climate data
for (i in 1: length(precip_files)){
  file_content <- readLines(precip_files[i])
  start <- which(grepl("2010-01-01 01Z", file_content))
  end <- which(grepl("2010-12-31 23Z", file_content))
  precip[,i] <- read.table(text = paste0(trimws(file_content[start : end]),collapse = "\n"))[,3]
  
  file_content <- readLines(pet_files[i])
  start <- which(grepl("2010-01-01 01Z", file_content))
  end <- which(grepl("2010-12-31 23Z", file_content))
  pet[,i] <- read.table(text = paste0(trimws(file_content[start : end]),collapse = "\n"))[,3]
  
  date <- read.table(text = paste0(trimws(file_content[start : end]),collapse = "\n"))[,1]
  
  rm(file_content,start,end)
}

#Process NLDAS climate data files
precip <- rowMeans(precip * 0.001) #Avg. across the catchment per time record (convert kg/m2 to m water)
precip <- data.frame("date" = as.Date(date,format='%Y-%m-%d'), "precip" = precip) #Input file
precip <- aggregate(precip ~ date, precip, sum) #total per day

pet <- rowMeans(pet * 0.001) #Avg. across the catchment per time record (convert kg/m2 to m water)
pet <- data.frame("date" = as.Date(date,format='%Y-%m-%d'), "pet" = pet) #Input file
pet <- aggregate(pet ~ date, pet, sum) #total per day
pet$pet <- ifelse(pet$pet<0,0,pet$pet)

#Load NWIS daily discharge data
file_content <- readLines(flow_file)
start <- which(grepl(unique(substr(as.character(date),1,5)), file_content))[1]
end <- rev(which(grepl(unique(substr(as.character(date),1,5)), file_content)))[1]
flow =  read.table(text = paste0(trimws(file_content[start : end]),collapse = "\n"))

#compile observations data frame
obs <- data.frame("date" = precip$date,
                  "flow" = flow[,4]*0.0283168, #ft3/sec to m3/s
                  "precip" = precip$precip, #m accumulated over time step
                  "pet" = pet$pet #m accumulated over time step
                  )

saveRDS(obs,"processed_obs.rds")

#rm(file_content,start,end,pet_files,precip_files,i,flow_file,pet,precip,flow)

