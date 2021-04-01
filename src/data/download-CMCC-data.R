# New Bioclim data: https://doi.org/10.1594/PANGAEA.904278

# Overall file data
download.file("https://doi.pangaea.de/10.1594/PANGAEA.904278?format=textfile",
              destfile = "data/external/CMCC-BioClimInd/Noce-etal_2019.tab")

# Has 35 bioclim variables
bio <- 1:35

# Download them 
library(devtools)
 
for (i in bio) {
  download.file(
    # the files url
    sprintf("https://hs.pangaea.de/model/NoceS-etal_2019/BIO%s.zip", i), 
    # destination            
    destfile = sprintf("data/external/CMCC-BioClimInd/BIO%s.zip", i))
  
  # Now unzip these zip files into their ownfolder
  utils::unzip(zipfile = sprintf("data/external/CMCC-BioClimInd/BIO%s.zip", i), 
               exdir = sprintf("data/external/CMCC-BioClimInd/BIO%s/", i))
  
  # remove the downloaded zip file
  file.remove(sprintf("data/external/CMCC-BioClimInd/BIO%s.zip", i))
}


# Dealing with netCDF files
library(ncdf4)

nc_data <- nc_open('data/external/CMCC-BioClimInd/BIO10/BIO10_HIST_1960_99.nc')
# 3dim file 
nc_data

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
time <- ncvar_get(nc_data, "time")

head(time)


























