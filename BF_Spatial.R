getwd() # check if your working directory is correctly set

install.packages("raster")
library(terra)

# The necessary add-on packages need to be installed within R before loading the 
# package using the library() function. Below we define a helper function that 
# installs the R package if it is not installed yet, and then also loads it using the library function:
  
  
# pkgTest is a helper function to load packages and install packages only when they are not installed yet.
pkgTest <- function(x)
{
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE)
  }
  library(x, character.only = TRUE)
}
neededPackages <- c("zoo", "bfast", "terra", "raster", "leaflet", "MODISTools")
for (package in neededPackages){pkgTest(package)}

# Loading extra function timeser() to create a time series object in R:
  
# Utility function to create time series object from a numeric vector
# val_array: data array for one single pixel (length is number of time steps)
# time_array: array with Dates at which raster data is recorded (same length as val_array)
timeser <- function(val_array, time_array) {
  z <- zoo(val_array, time_array) # create zoo object
  yr <- as.numeric(format(time(z), "%Y")) # extract the year numbers
  jul <- as.numeric(format(time(z), "%j")) # extract the day numbers (1-365)
  delta <- min(unlist(tapply(jul, yr, diff))) # calculate minimum time difference (days) between observations
  zz <- aggregate(z, yr + (jul - 1) / delta / 23) # aggregate into decimal year timestamps
  (tso <- as.ts(zz)) # convert into timeseries object
  return(tso)
}
# Downloading MODIS data using the MODISTools package
# First we download the MODIS data via the mt_subset function:
# Downloading the NDVI data, starting from 2000-01-01
VI <- mt_subset(product = "MOD13Q1",
                site_id = "nl_gelderland_loobos",
                band = "250m_16_days_NDVI",
                start = "2000-01-01",
                end = "2022-03-22",
                km_lr = 2,
                km_ab = 2,
                site_name = "testsite",
                internal = TRUE,
                progress = TRUE)

## Downloading chunks:
# Downloading the pixel reliability data, starting from 2000-01-01
QA <- mt_subset(product = "MOD13Q1",
                site_id = "nl_gelderland_loobos",
                band = "250m_16_days_pixel_reliability",
                start = "2000-01-01",
                end = "2022-03-22",
                km_lr = 2,
                km_ab = 2,
                site_name = "testsite",
                internal = TRUE,
                progress = TRUE)

# Creating a raster brick and cleaning the MODIS data using the reliability layer
# Second, we create a raster brick using the mt_to_terra function that is included in the new MODISTools package (version 1.1.4).
# convert df to rasterThird, we clean the MODIS NDVI data using pixel reliability information:
VI_r <- mt_to_terra(df = VI)
QA_r <- mt_to_terra(df = QA)


# Now check also the pixel reliability information in Table 4 available via the following link to the MODIS VI User Guide c6 version. 
# This is important to understand how this works for the following question!
#   
## clean the data
# create mask on pixel reliability flag set all values <0 or >1 NA
m <- QA_r
m[(QA_r < 0 | QA_r > 1)] <- NA # continue working with QA 0 (good data), and 1 (marginal data)

# apply the mask to the NDVI raster
VI_m <- mask(VI_r, m, maskvalue=NA, updatevalue=NA)

# plot the first image
plot(m,9) # plot mask of time step 9

plot(VI_m,9) # plot cleaned NDVI raster

# You can (optional!) extract data from the cleaned VI 
# raster brick via the click function. Note: it will wait 
# for you to click on the plot to select the pixel whose values it will print! While it’s waiting for you, no other code can be run. If you want to cancel without clicking, press the Esc button on your keyboard.

# extract data from the cleaned raster for selected pixels
click(VI_m, id=TRUE, xy=TRUE, cell=TRUE, n=1)

library(leaflet)
r <- raster(VI_m[[1]]) # Select only the first layer (as a RasterLayer)
pal <- colorNumeric(c("#ffffff", "#4dff88", "#004d1a"), values(r),
                    na.color = "transparent")

map <- leaflet() %>% addTiles() %>%
  addRasterImage(r, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(r),
            title = "NDVI")
map

# Below we extract the data from the raster as a vector and create a time series using the timeser function:
## check VI data at a certain pixel e.g. 1 row, complete left hand site:
## the dimensions of the raster are: 33x33

px <- 78 # pixel number; adjust this number to select the center pixel
tspx <- timeser(unlist(VI_m[px]),as.Date(names(VI_m), "%Y-%m-%d")) # convert pixel "px" to a time series
plot(tspx, main = 'NDVI') # NDVI time series cleaned using the "reliability information"
# Now we are ready to detect breaks in the time series! 
# You can now choose: either use BFAST Monitor (“Option 1”, the following section) to detect a single break at the end of the time series, or use BFAST Lite (“Option 2”, the section after that) to detect all breaks in the middle of the time series. If you are interested, you can do both, but it’s not necessary to answer both sets of questions.

# Option 1: detect break at the end of the time series with BFAST Monitor
# Now we apply the bfastmonitor function using a trend + harmon model with order 3 for the harmonics (i.e. seasonality modelling):
bfm1 <- bfastmonitor(tspx, response ~ trend + harmon, order = 3, start = c(2018,3)) # Note: the third observation in 2018 marks the transition from 'history' to 'monitoring'
plot(bfm1)

# Let’s run the bfastmonitor code on the full raster time series spatially using the app function:
dates <- as.Date(names(VI_m), "%Y-%m-%d")

# here we define the function that we will apply across the brick using the `app` function:
bfmRaster = function(pixels)
{
  tspx <- timeser(pixels, dates) # create a timeseries of all pixels
  bfm <- bfastmonitor(tspx, response ~ trend + harmon, order = 3, start = c(2019,1)) # run bfast on all pixels
  return(c(bfm$breakpoint, bfm$magnitude)) 
}

# apply the function to each raster cell (time series)
# Optionally you can supply an argument cores=n (where n is the number of cores on your computer) for a potential speed boost
bfmR <- app(VI_m, bfmRaster)
names(bfmR) <- c('time of break', 'magnitude of change')
plot(bfmR) # resulting time and magnitude of change

# Here is example R code to get the pixel number. When you run click(), click in the plot on a pixel with a break (i.e. where an estimated time of break is available).

plot(bfmR,1)
click(VI_m, id=FALSE, xy=FALSE, cell=TRUE, n=1)
# Here we selected one pixel, and do the bfastmonitor analysis for that pixel:
  
px <- 460 # pixel number so adjust this number to select the center pixel
tspx <- timeser(unlist(VI_m[px]),as.Date(names(VI_m), "%Y-%m-%d")) # convert pixel 1 to a time series
plot(tspx, main = 'NDVI') # NDVI time series cleaned using the "reliability information"

tspx[tspx < 0] <- NA
bfm <- bfastmonitor(tspx, response ~ trend + harmon, order = 3, start = c(2019,2))
plot(bfm)

