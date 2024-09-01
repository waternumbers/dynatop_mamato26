#------------------------
#Setup Dynamic TOPMODEL simulations
#------------------------

#For more information:
#https://waternumbers.github.io/dynatop/

#------------------------
#Getting Started
#------------------------
rm(list=ls())

demo_dir <- "./test_project"

#Install and load dependencies
#devtools::install_github("waternumbers/dynatop")
library("dynatop")
library("xts")

#Load model created with dynatopGIS
dynatop_drb_model <- readRDS(file.path(demo_dir,"new_model.rds"))
#Load map created with dynatopGIS
dynatop_drb_map <- terra::rast(file.path(demo_dir,"new_model.tif"))


#------------------------
#Preparing input data
#------------------------

obs <- readRDS("processed_obs.rds")
#Convert to xts object format
xts.obs <- xts(obs[,c("flow","precip","pet")], order.by = as.POSIXct(obs$date)) ## THIS IS THE CHANGE
#xts.obs <- xts.obs[90:180,]


#------------------------
#Altering parameters
#------------------------

hru <- dynatop_drb_model$hru
for(i in 1:length(hru)){
  if(is.na(hru[[i]]$class$endNode)){
    ## then HRU is not a channel
    ## saturated zone parameters
    hru[[i]]$sz$parameters["m"] <- 0.063 #Exponential scaling parameter for the decline of transmissivity with increase in storage deficit (1/m)
    hru[[i]]$sz$parameters["t_0"] <- 1*10*(1/(24*60*60)) #Downslope transmissivity when the soil is just saturated to the surface (m2/s 
    ## unsaturated zone parameters
    hru[[i]]$uz$parameters["t_d"] <- 8*60*60 #time delay for recharge to the saturated zone per unit deficit (s/m)
    ## root zone parameters
    hru[[i]]$rz$parameters["s_rzmax"] <- 0.15 #Maximum capacity of the root zone (available water capacity to plants) (m)
    ## surface parameters
      hru[[i]]$sf$parameters["c_sf"] <- 1 #Channel routing wave velocity (celerity) (m/s)
      ##hru[[i]]$sf$parameters["d_sf"] <- (hru[[i]]$sf$parameters["c_sf"]*hru[[i]]$properties["Dx"])/2 
  }else{
    ## then HRU is a channel - set so no subsurface response
    ## saturated zone parameters
    hru[[i]]$sz$parameters["t_0"] <- 0.001
    ## root zone parameters
    hru[[i]]$rz$parameters["s_rzmax"] <- 0.001
    ## surface parameters
      hru[[i]]$sf$parameters["c_sf"] <- 0.4 #m/s
      hru[[i]]$sf$parameters["d_sf"] <- (hru[[i]]$sf$parameters["c_sf"]*hru[[i]]$properties["Dx"])/2 
  }
  ## initialization parameters
  hru[[i]]$initialisation["s_rz_0"] <- 0.98
  hru[[i]]$initialisation["r_uz_sz_0"] <- obs$flow[1]/1e8 ## initial outflow divided by catchment area
}

#------------------------
#Creating the dynatop Object
#------------------------

#create model
ctch_mdl <- dynatop$new(hru,map=dynatop_drb_model$map)
#add time series observations to model
ctch_mdl$add_data(xts.obs)

#------------------------
#Running dynamic TOPMODEL
#------------------------
states <- list()
##Initialize model with recharge rate
ctch_mdl$initialise()$plot_state("s_sz")
states[[1]] <- ctch_mdl$get_states()

       
#Perform initial simulation
sim1 <- ctch_mdl$sim(dynatop_drb_model$output_flux)$get_output()
##Save final model state 
ctch_mdl$plot_state("s_sz")


## get initial output
## Re-run with previous model save state
sim2 <- ctch_mdl$sim(dynatop_drb_model$output_flux)$get_output()

#Extract model output and merge with observations
out <- merge( merge(xts.obs,sim1),sim2)
names(out) <- c(names(xts.obs),'sim_1','sim_2')

#------------------------
#Plot output
#------------------------

#Plot predictions against observations
plot(out[,c('flow','sim_1','sim_2')], main="Discharge",ylab="m3/s",legend.loc="topright")

#Plot mass balance error
mb <- ctch_mdl$get_mass_errors()
#plot( mb[,6] , main="Mass Error", ylab="[m^3]")


