set.seed(123)
# Clearing Environment
rm(list = ls())
# Working Directory
getwd()
# Setting Working Directory
setwd("D:\\GWR_NFHS_Assam")
# Verifying Working Directory
getwd()

# Packages
library(tidyverse)  # for data wrangling and operations
library(sf)         # for spatial data
library(tidyverse)  # for data wrangling
library(cols4all)   # for graphics
library(tmap)       # for mapping
library(GWmodel)    # for GWR Regression
library(cowplot)    # for manipulating graphics
library(sp)

# Importing the Processed Dataset
df_pcs <- read_csv("Processed_dataset.csv")
head(df_pcs)
# Subsetting the dataframe to contain only districts from Assam
df_assam <- df_pcs %>%
  filter(State == "Assam")

# GPKG file taken from https://github.com/sssmartsearch/India_Boundary_Updated/tree/master
map_assam = st_read("stname_ASSAM.gpkg")
map_assam
ggplot() + 
  geom_sf(data = map_assam, fill = "lightblue", colour = "white")

# Changing the district identifier name
map_assam <- rename(map_assam, District = dtname)

# Changing district names for easier joining
map_assam <- map_assam %>%
  mutate(District = if_else(District == "Kamrup Metropolitan", "Kamrup Metro", District))
df_assam <- df_assam %>%
  mutate(District = if_else(District == "Marigaon", "Morigaon", District))

# Joiniing the tibble and spatial object
assam_sf <- map_assam %>% inner_join(df_assam) 
colnames(assam_sf)
# removing unwanted columns
assam_sf_rev <- assam_sf %>%
  dplyr::select(-stname, -stcode11, -dtcode11, -year_stat, -Dist_LGD,
         -State_LGD, -JID, -layer, -path, -State)

# Map of Anaemia prevalence leve;s
assam_sf_rev %>% ggplot() + geom_sf(aes(fill = Women_Anaemic_15_49), colour = NA) +
  scale_fill_continuous(name = "% Women anaemic between 15-49 years", type = "viridis")
# Using tmap
boundary = st_union(assam_sf_rev)
# Normal TMAP
tm_shape(assam_sf_rev)+
  tm_fill("Women_Anaemic_15_49", title = "% Women anaemic between 15-49 years", 
          palette = "Reds", legend.title.size = 2,
          style = "kmeans", legend.hist = T)+
  tm_layout(title = "Assam",
            frame = F, legend.outside = T, 
            legend.hist.width = 1,
            legend.format = list(digits = 1), 
            legend.outside.position = c("left", "top"),
            legend.text.size = 1,
            legend.title.size = 0.7)+
  tm_compass(position = c(0.13, 0.1))+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary) + tm_borders(col = "black", lwd = 1.5)
tmap_mode("plot")
tmap_mode("view")
tm_shape(assam_sf_rev)+
  tm_fill("Women_Anaemic_15_49", title = "% Women anaemic between 15-49 years", 
          palette = "Reds", legend.title.size = 2,
          style = "kmeans", legend.hist = T)+
  tm_layout(title = "Assam",
            frame = F, legend.outside = T, 
            legend.hist.width = 1,
            legend.format = list(digits = 1), 
            legend.outside.position = c("left", "top"),
            legend.text.size = 1,
            legend.title.size = 0.7)+
  tm_compass(position = c(0.13, 0.1))+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary) + tm_borders(col = "black", lwd = 1.5) + tm_basemap('Esri.WorldImagery')
tmap_mode("plot")

#----------Multiscale GWR Regression-------------
colnames(assam_sf_rev)
# type
str(assam_sf_rev)
# converting to spatial dataframe
assam_sp <- as_Spatial(assam_sf_rev)
gw.ms <- gwr.multiscale(Women_Anaemic_15_49 ~ Children_Anaemic_6_59_Months + 
                          Women_High_Blood_Sugar + 
                          Teen_Mothers_15_19 + 
                          Women_BMI_Below_Normal + 
                          Children_Underweight_U5 + 
                          Women_Consume_Alcohol_15Plus + 
                          Female_Ever_Attended_School_6Plus + 
                          Married_Women_Using_Pill_15_49,
                        data = assam_sp,
                        adaptive = T, max.iterations = 3000,
                        criterion="CVR",
                        kernel = "bisquare",
                        bws0= rep(1000, 9),
                        verbose = F, predictor.centered=rep(F, 8))
gw.ms
# Checking bandwidth
data.frame(VarName = names(gw.ms$SDF)[1:length(gw.ms[[2]]$bws)], 
           MSGWR_bw = gw.ms[[2]]$bws)
# Children’s anaemia, blood sugar, and teen pregnancy are strong and consistent predictors
# Alcohol use shows high local variation — investigate further (outliers or spatial clustering)
# Education and contraceptive use are negatively associated — consistent with social determinants literature

# Plugging these bandwidths into the model

gw.res = gwr.multiscale(Women_Anaemic_15_49 ~ Children_Anaemic_6_59_Months + 
                          Women_High_Blood_Sugar + 
                          Teen_Mothers_15_19 + 
                          Women_BMI_Below_Normal + 
                          Children_Underweight_U5 + 
                          Women_Consume_Alcohol_15Plus + 
                          Female_Ever_Attended_School_6Plus + 
                          Married_Women_Using_Pill_15_49, 
                        data = assam_sp,
                        adaptive = T,
                        criterion="CVR", kernel = "bisquare", 
                        bws0=gw.ms[[2]]$bws,
                        bw.seled=rep(T, 8),
                        verbose = F, predictor.centered=rep(F, 8))

# Table of MGWR Coefficients
t1 <- apply(
  gw.res$SDF@data |>
    dplyr::select(Intercept:Married_Women_Using_Pill_15_49), 2,
  summary)
t2 = gw.ms[[2]]$bws
# joining together with a row bind
tab <- rbind(t1, t2)
# adding name to last row of tab
rownames(tab)[7] <- "BW"
# transpose tab
tab <- t(round(tab, 3))
tab


gw_sf <- st_as_sf(gw.res$SDF)
p1 <- ggplot(gw_sf) +
  geom_sf(aes(fill = Children_Anaemic_6_59_Months), col = NA) +
  scale_fill_continuous(type = "viridis", direction = -1, name = "MGWR") +
  # Add significant polygons
  geom_sf(data = gw_sf[signif_1, ], fill = NA, col = "white") +
  theme(legend.position = "bottom")
plot(p1)

# Plotting
tval_1 = gw.res$SDF$Children_Anaemic_6_59_Months_TV
#signif_1 = tval_1 < -1.96 | tval_1 > 1.96
# 90% CI
signif_1 = tval_1 < -1.645 | tval_1 > 1.645
# create the map 
p1 <- ggplot(gw_sf) +
  geom_sf(aes(fill = Children_Anaemic_6_59_Months), col = NA) +
  scale_fill_continuous(type = "viridis", direction = -1, name = "MGWR") +
  geom_sf(data = gw_sf[signif_1, ], fill = NA, col = "white") +
  theme(legend.position = "bottom")
plot(p1)

save.image(file = "gwr_nfhs_assam_modelling.RData")
