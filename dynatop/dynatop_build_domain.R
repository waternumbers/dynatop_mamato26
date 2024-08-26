#------------------------
#Build Domain for Dynamic TOPMODEL
#------------------------

#For more information:
#https://waternumbers.github.io/dynatopGIS/

#------------------------
#Getting Started
#------------------------

#Install and load dependencies
#devtools::install_github("waternumbers/dynatopGIS")
library("dynatopGIS")
library("terra")
library("sf")
library("readxl")

#Set working directory
wd <- "./dynatop/" ## assume starting in root of the github repository

#Create temporary directory for model
demo_dir <- "test_project" ## don't want project in tmp - just used in vignette to pass CRAN checks tempfile("dynatop_drb")
dir.create(demo_dir)

#Initialize model object specifying the location of the meta data file
ctch <- dynatopGIS$new(file.path(demo_dir))

#------------------------
#Add catchment data
#------------------------

#Load DEM file (raster)
dem <- terra::rast(list.files(wd,pattern="DEM*",full.names=TRUE))

#Load catchment boundary (shapefile)
huc <- read_sf(list.files(wd,pattern="HUC*.shp",full.names=TRUE))
huc <- sf::st_transform(huc, crs(dem)) # ensure crs projections are matching

#Crop DEM to catchment boundary
dem <- terra::mask(crop(dem,ext(huc)),huc)
dem <- terra::extend(dem,1) # pad with NA values

#Load channel information and shapefile (with attributes)
channel_info <- readxl::read_excel(list.files(wd,pattern="Channel.+xlsx",full.names=TRUE))
shp <- sf::read_sf(list.files(wd,pattern="Channel.+.shp",full.names=TRUE))
shp <- sf::st_transform(shp, crs(dem)) #ensure crs projections are matching

#Process channel
shp$width <- ifelse(channel_info$Width_m==0,mean(channel_info$Width_m),channel_info$Width_m)
shp$length <- channel_info$Length_km*1000
shp$startNode <-  sub(":.*", "", channel_info$Segment)
shp$endNode <-  channel_info$Downstream_ID
shp$slope <-  ifelse(channel_info$slope==0,mean(channel_info$slope),channel_info$slope)
sp_lines <- terra::vect(shp) #convert to vector
property_names <- c(name="WASPNAME",
                    endNode="endNode",
                    startNode="startNode",
                    length="length",
                    width="width",
                    slope="slope")
chn <- dynatopGIS::convert_channel(sp_lines,property_names)

#Load or specify catchment (and if necessary sub-catchment) outline
catchment_outline <- terra::ifel(is.finite(dem),1,NA)

#Add catchment/DEM/channel information to dynatop model object
ctch$add_catchment(catchment_outline) #must add first
ctch$add_dem(dem)
ctch$add_channel(chn)

#Visualize model domain
ctch$plot_layer("dem", add_channel=TRUE)

#------------------------
#Build model domain
#------------------------

#Fill in localized sinks so all flow paths drain to the river
ctch$sink_fill()
terra::plot( ctch$get_layer('filled_dem') - ctch$get_layer('dem'),
             main="Changes to height")

#Determine ordering of hydrologic response units (HRUs)
ctch$compute_band()
ctch$plot_layer("band")

#Compute topographic index: ln(upslope area/tan(gradient)) or ln(a/tan b)
ctch$compute_properties()
ctch$plot_layer('atb')

#Compute flow lengths
ctch$compute_flow_lengths(flow_routing="expected")
ctch$plot_layer("expected_flow_length")

#Add additional layer (optional: land greater/less than 1000 m)
#tmp <- ctch$get_layer("filled_dem")
#tmp <- terra::ifel(tmp<=800,NA,-999)
#ctch$add_layer(tmp, "filter")

#Divide hillsope into n+1 classes (Optional)
ctch$classify("atb_class","atb",cuts=20)
ctch$plot_layer("atb_class")

#Combine atb classes with distance bands (Optional)
ctch$combine_classes("atb_class_band",c("atb_class","band"))
ctch$plot_layer("atb_class_band")

#Burn in (i.e., filter) by additional layer (Optional) 
#ctch$combine_classes("atb_class_band_filter",pairs=c("atb_class","band"),burns="filter")
#ctch$plot_layer("atb_class_band_filter")

#------------------------
#Create model object
#------------------------

#Use band distance metric to create model
ctch$create_model(file.path(demo_dir,"new_model"),"atb_class_band")
#Check  if tiff and rds files were successfully created
list.files(demo_dir,pattern="new_model*")

##rm(catchment_outline,channel_info,chn,dem,shp,sp_lines,property_names,huc)



