library(ncdf4)
library(raster)
library(tidyr)
library(dplyr)
library(rgdal)
library(sf)

setwd("/Users/amuhebwa/Documents/Python/Predicting_River_Discharge/Create_Dataset/R_Code_DataExtraction/")

spatial_extraction <- function(file,varname,res_meters,projection_string,basin_projected,shape_extent_buffered){
  #read it in. R can read netcdf to raster directly
  variable=raster(file, level =1, varname=varname)
  #buffer that extent and crop
  variable_cropped=crop(variable,shape_extent_buffered)
  variable_projected= projectRaster(from=variable_cropped,res=c(res_meters,res_meters),crs= CRS(projection_string))
  #extract the values within the shapfile
  variable_masked=mask(variable_projected,basin_projected)
  spatially_queried_pixel_data=unlist(raster::extract(variable_projected,basin_projected,method='simple'))
  #---
  return(spatially_queried_pixel_data)
}


varname='AvgSurfT_inst'
res_meters=15000
projection_string='+init=EPSG:32611'

basin_of_interest <- st_read("basin_10BE010/basin_10BE010.shp")
name2save <-file.path(getwd(), 'netcdf_reduced_basin_10BE010.csv')
shape_extent=extent(basin_of_interest )
# add one degree to each edge
shape_extent_buffered= shape_extent +50

#project the basin now
st_crs(basin_of_interest)=crs('+init=EPSG:4326')
basin_projected <- st_transform(basin_of_interest,CRS(projection_string))

daily_folders <- sort(list.files(path=file.path(getwd(),'daily_datasets'), full.names=TRUE, pattern = "1981"))

temperature_values <- vector("list", length(daily_folders))
matching_dates <- vector("list", length(daily_folders))
i <- 1 # appending by index
for (daily_folder in daily_folders){
  daily_files <- list.files(path = daily_folder, full.names = TRUE, pattern = "nc4")
  daily_files <- sort(daily_files)
  #print(daily_files)
  
  
  #sapply lets us use lapply syntax but auto simplifies the output
  spatial_pixel_data <- sapply(daily_files, spatial_extraction, varname=varname,res_meters=res_meters,
                             projection_string=projection_string,basin_projected=basin_projected,
                             shape_extent_buffered=shape_extent_buffered, simplify = TRUE) %>%
    rowMeans()
  
  spatial_extraction_spatial_average <- mean(spatial_pixel_data)
  current_Date <- sapply(strsplit(daily_folder, "/"), tail, 1)
  print(spatial_extraction_spatial_average)
  print(current_Date)
  temperature_values[[i]] <- spatial_extraction_spatial_average
  matching_dates[[i]] <- current_Date
  i <- i + 1
  print('----------------------------------')
  
  rm(daily_folder)
  rm(daily_files)
  rm(spatial_pixel_data)
  rm(spatial_extraction_spatial_average)
  rm(current_Date)
}

combined_list <- list("Benchnark_Avg_temp" = temperature_values, "Date" = matching_dates)
final_df <- as.data.frame(do.call(cbind, combined_list))
final_df <- data.frame(lapply(final_df, as.character), stringsAsFactors=FALSE)
write.csv(final_df, name2save)
#print(final_df)
