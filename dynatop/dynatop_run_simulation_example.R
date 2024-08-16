#------------------------
#Setup Dynamic TOPMODEL simulations
#------------------------

#For more information:
#https://waternumbers.github.io/dynatop/

#------------------------
#Getting Started
#------------------------

#Install and load dependencies
#devtools::install_github("waternumbers/dynatop")
library("dynatop")
library("xts")

data("Swindale")


#------------------------
#Preparing input data
#------------------------

#Convert to xts object format
xts.obs <- Swindale$obs
#xts.obs <- xts.obs[90:180,]


#------------------------
#Altering parameters
#------------------------

hru <- Swindale$model$hru
for(i in 1:length(hru)){
  if(is.na(hru[[i]]$class$endNode)){
    ## then HRU is not a channel
    ## saturated zone parameters
    hru[[i]]$sz$parameters["m"] <- 0.0063 #Exponential scaling parameter for the decline of transmissivity with increase in storage deficit (1/m)
    hru[[i]]$sz$parameters["t_0"] <- exp(7.73) #Downslope transmissivity when the soil is just saturated to the surface (m2/s)
    ## unsaturated zone parameters
    hru[[i]]$uz$parameters["t_d"] <- 8*60*60 #time delay for recharge to the saturated zone per unit deficit (s/m)
    ## root zone parameters
    hru[[i]]$rz$parameters["s_rzmax"] <- 0.1 #Maximum capacity of the root zone (available water capacity to plants) (m)
    ## surface parameters
    hru[[i]]$sf$parameters["c_sf"] <- 0.4 #Channel routing wave velocity (celerity) (m/s)
  }else{
    ## then HRU is a channel - set so no subsurface response
    ## saturated zone parameters
    hru[[i]]$sz$parameters["t_0"] <- 0.001
    ## root zone parameters
    hru[[i]]$rz$parameters["s_rzmax"] <- 0.001
    ## surface parameters
    hru[[i]]$sf$parameters["c_sf"] <- 0.8 #m/s
  }
  ## initialization parameters
  hru[[i]]$initialisation["s_rz_0"] <- 0.98
  hru[[i]]$initialisation["r_uz_sz_0"] <- 1e-7 ## initial outflow divided by catchment area
}

#------------------------
#Creating the dynatop Object
#------------------------

#create model
ctch_mdl <- dynatop$new(hru,map=system.file("extdata","Swindale.tif",package="dynatop",mustWork=TRUE))
#add time series observations to model
ctch_mdl$add_data(xts.obs)

#------------------------
#Running dynamic TOPMODEL
#------------------------

#Initialize model with recharge rate
ctch_mdl$initialise()$plot_state("s_sz")
#Perform initial simulation
sim1 <- ctch_mdl$sim(Swindale$model$output_flux)$get_output()
#Save final model state 
ctch_mdl$plot_state("s_sz")
#Re-run with previous model save state
for ( i in 1:10){
  sim2 <- ctch_mdl$sim(Swindale$model$output_flux)$get_output()
}

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
plot( mb[,6] , main="Mass Error", ylab="[m^3]")


