# Calculating-AGB-change-within-fire-boundaries-in-CA
This code takes a biomass raster time seires, and overlays fire polygons, filtering them by year, to calculate pre-fire, post-fire and biomass change per year, overtime. 
##### Prep #####
# Load Libraries
library(dplyr)
library(terra)
library(ggplot2)
```
# Lets start preparing files for a larger analysis
```{r}
# Workflow: 
# All in a for loop 
# 1. filter MTBS for year
# 2. extract pre fire and post fire and difference
# 3. Save those values to CSV
# 4. plot! 

# CONVERSION INFO # In the CECS dataset, each 30x30 meter pixel represents an area of 0.09 hectares. The value (X) in each pixel indicates biomass density in units of 10 Mg/Ha (a value of 2000 indicates 20 kg/m2, or 200 Mg/ha) Therefore, to find the true biomass in Megagrams (Mg) for each pixel, we need to convert the density value to Mg/Ha by deviding by 10, and then multiply by the pixel area in hectares.

  # Set paths and load in files
MTBS <- vect("/Users/hopewolfchild/Desktop/Data/Fire/MTBS Burned Areas Boundaries 1984-2022/mtbs_perimeter_data/mtbs_perims_DD.shp")
CECS_path <- ("/Users/hopewolfchild/Desktop/Data/CECS/LAGCCA/")
CECS <- rast(paste0(CECS_path, "CAWide_LiveAboveC_", CECS_years, ".tif"))
Sample_raster <- rast(paste0(CECS_path, "CAWide_LiveAboveC_1985.tif"))
plot(Sample_raster)
  # initialize years
MTBS_years <- (1986:2022)
CECS_years <- (1985:2023)
  # Make sure MTBS and CECS are on the same CRS
MTBS <- project(MTBS, crs(CECS))
head(MTBS)

  # initialize Data Frame, we want to include pre-fire, post-fire, and biomass loss
agc_loss_MTBS <- data.frame(Year = MTBS_years, PreFire= NA, PostFire = NA, Biomass_Loss = NA, Hectare_Burned = NA, Avg_MgC_Loss_Per_Hectare = NA)
print(agc_loss_MTBS)

  # prepare MTBS by adding a new column: "Year"
  # just getting the date in the right format
MTBS$Ig_Date <- as.Date(MTBS$Ig_Date, format="%Y/%m/%d") 
  # adding Year column
MTBS$Year <- format(MTBS$Ig_Date, "%Y")


######## run the analyses ##################################################
for (year in MTBS_years){ 
  
  # Step 1. Filter MTBS for year
MTBSyear <- subset(MTBS, MTBS$Year == year)
  # load in prefire year and post fire year
pre_rast <- rast(paste0(CECS_path, "CAWide_LiveAboveC_", year-1, ".tif"))
post_rast <- rast(paste0(CECS_path, "CAWide_LiveAboveC_", year+1, ".tif"))
sum(post_rast)
print(paste0("Working on year: ", year))
  # start the calculations!
  
  # pre fire biomass, conversion and adding to data frame
Prefire_pixel <- extract(pre_rast, MTBSyear, na.rm = TRUE)
Prefire_pixel <- sum(Prefire_pixel, na.rm = TRUE)
Prefire <- (Prefire_pixel / 10) * 0.9
print(paste0("Prefire biomass: ", Prefire))
row_index <- which(agc_loss_MTBS$Year == year)
agc_loss_MTBS$PreFire[row_index] <- Prefire

  # post fire biomass
Postfire_pixel <- extract(post_rast, MTBSyear, na.rm = TRUE)
Postfire_pixel <- sum(Postfire_pixel, na.rm = TRUE)
Postfire <- (Postfire_pixel / 10) * 0.9
print(paste0("Postfire biomass: ", Postfire))
agc_loss_MTBS$PostFire[row_index] <- Postfire
  
  # biomass loss
Biomass_loss <- Prefire - Postfire
agc_loss_MTBS$Biomass_Loss[row_index] <- Biomass_loss
print(paste0("Biomass Loss: ", Biomass_loss))
  # hectare burned and avg Megagram loss per hectare
  # make a new raster of that fire year to count how many pixels
PixelCountRast <- mask(pre_rast, MTBSyear)
PixelCount <- global(PixelCountRast, fun = "notNA")
Hectare_burned <- as.numeric(PixelCount * 0.9)
  # add to data frame
agc_loss_MTBS$Hectare_Burned[row_index] <- Hectare_burned
  # average
Avg_Loss_Per_Hectare_Burned <- Biomass_loss/Hectare_Burned
  # add to dataframe
agc_loss_MTBS$Avg_MgCLoss_Per_Hectare[row_index] <- Avg_Loss_Per_Hectare_Burned
print(paste0("Year complete: ", year))
print("")
}
###############################################################################
agc_loss_MTBS

  # save the data frame
#write.csv(agc_loss_MTBS, file = "/Users/hopewolfchild/Desktop/Fire_BiomassLoss_CA_86-22.csv")
file.choose()
  # plotting
Fire_Biomass_data <- read.csv("/Users/hopewolfchild/Desktop/Data/CECS/Fire_BiomassLoss_CA_86-22.csv")
California_Biomass <- read.csv("/Users/hopewolfchild/Desktop/Data/CECS/LAGCCA/ca_bm_perpixel.csv")
California_Biomass
Fire_Biomass_data

# fit and get slopes
lm_model_Burned <- lm(Hectare_Burned / 1e6 ~ Year, data = Fire_Biomass_data)
slope_burned <- coef(lm_model_Burned)[2]  # Extract slope
std_burned <- 
lm_model_Biomass <- lm(TotalBiomass / 1e9 ~ Year, data = California_Biomass)
slope_Biomass <- coef(lm_model_Biomass)[2]  # Extract slope
std_Biomass <- 

ggplot() + 
  # Plot biomass on primary y-axis
  geom_point(data = California_Biomass, aes(x = Year, y = TotalBiomass / 1e9), col = "green4") +
    # Add a linear trend line for biomass
  geom_smooth(data = California_Biomass, aes(x = Year, y = TotalBiomass/ 1e9), 
              method = "lm", linetype = "dashed", col = "gray", se = FALSE) + 
  # Plot fire data on secondary y-axis (scaled to align with primary axis)
  geom_point(data = Fire_Biomass_data, aes(x = Year, y = Hectare_Burned / 1e6), col = "red3") + 
  # Add a linear trend line for hectares burned
  geom_smooth(data = Fire_Biomass_data, aes(x = Year, y = Hectare_Burned / 1e6), 
              method = "lm", linetype = "dashed", col = "gray", se = FALSE) + 
  
    # Add annotation for the slope of burned
  annotate("text", x = min(Fire_Biomass_data$Year) + 2, 
           y = max(Fire_Biomass_data$Hectare_Burned / 1e6) - 10, 
           label = paste0("Slope: ", round(slope_burned, 2)), 
           color = "black", hjust = 0) +
  
   # Add annotation for the slope of biomass
  annotate("text", x = min(California_Biomass$Year) + 2, 
           y = max(California_Biomass$TotalBiomass / 1e9) - 4, 
           label = paste0("Slope: ", round(slope_Biomass, 2)), 
           color = "black", hjust = 0) +
   # Define y-axis scales
  scale_y_continuous(
    name = "Live Aboveground Biomass (Billion Mg C)",
    sec.axis = sec_axis(~.*1, name = "Hectares Burned (Millions)")) +
  # Labels and theme
  labs(title = "Trend in Live Aboveground Biomass in California (1985-2022)",
       x = "Year", 
       caption ="Figure 1: Live Aboveground Biomass and fire dynamics in California (1985-2022). The green points represent \n biomass over time (billion Mg C), and the red points represent hectares burned (millions) The gray trend lines \n shows a linear increase in hectares burned over time (0.15 Million per year), and a linear decrease in biomass \n overtime (-0.01 billion megagrams per year)."
  ) +
  theme_minimal() +
  theme( plot.caption = element_text(size = 10, hjust = 0),
         plot.caption.position = "plot")  # Keeps caption within plot boundaries
```
