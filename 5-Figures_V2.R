library(tidyverse)
library(marmap)
library(ggmap)
library(viridis)
library(mapdata)
library(sf)
library(raster)
library(cowplot)
library(gridExtra)
library(scales)
library(feather)

# ToDO ---------------------------------

# ** Fix titles for all plots


# 10. Fishing Hours Regression Latitudinal Gradient 
# 12. Shannon Diversity error (prediction now - obs)

# 13. Shannon Diversity difference maps
# 14. Shannon Diversity Regression Latitudinal Gradient 


# 1. Table + text of Random forest deets




# Complete -------------------------------------------
# 1. Map of Global Fishing Effort (log fighing hours/mmsi_count)
# 2. Maps of Global Fishing Effort for Chinese, US, Spanish, TWN, and Japanese
# 3. Spatial overlap maps for CHN, USA, ESP, JPN, TWN
# 4. Maps of shannon diversity continous globally
# 5. Maps of shannon diversity binned globally - zero, low, med and high div
# 6. ESM data: mean and var SST, Chl and one other globally
# 7. Fishing Hours continuous regression predicted values now and the future
# 8. Fishing Hours error (prediction now - obs)
# 9. Fishing hours difference maps


# 11. Shannon diversity classiciation predicted values now and the future


# -------------------------------------------------------






# -------------------------------------------------------
# Setup
# -------------------------------------------------------

setwd("~/Projects/World-fishing-rich-diver-equit/")

# World polygons from the maps package
world_shp <- sf::st_as_sf(maps::map("world", wrap = c(0, 360), plot = FALSE, fill = TRUE))

# Load EEZ polygons
eezs <- read_sf("data/World_EEZ_v11_20191118_HR_0_360/", layer = "eez_v11_0_360") %>% 
  filter(POL_TYPE == '200NM') # select the 200 nautical mile polygon layer

mpas <- read_sf('data/mpa_shapefiles/vlmpa.shp')
mpas <- st_shift_longitude(mpas)

rfmos <- read_sf('data/RFMO_shapefile/RFMO_coords.shp')

# Specific color pallete
nipy_spectral <- c("#000000", "#6a009d", "#0035dd", "#00a4bb", "#009b0f",
                   "#00e100", "#ccf900", "#ffb000", "#e50000")

color_scheme <- c("#000000","#18001b","#300037","#480052","#60006e","#770088","#7b008c","#7e008f","#810092","#850096","#850099",
  "#6a009d","#4e00a0","#3300a4","#1700a7","#0000ac","#0000b6","#0000c0","#0000ca","#0000d5","#0005dd","#001ddd",
  "#0035dd","#004ddd","#0065dd","#0079dd","#0080dd","#0086dd","#008ddd","#0094dd","#009ada","#009dd0","#00a1c5",
  "#00a4bb","#00a8b1","#00aaa8","#00aaa1","#00aa9a","#00aa93","#00aa8c","#00a97d","#00a562","#00a246","#009e2b",
  "#009b0f","#009c00","#00a300","#00aa00","#00b100","#00b800","#00be00","#00c500","#00cc00","#00d300","#00da00",
  "#00e100","#00e800","#00ef00","#00f500","#00fc00","#17ff00","#3cff00","#62ff00","#88ff00","#aeff00","#c2fd00",
  "#ccf900","#d6f600","#e1f200","#ebef00","#f0e900","#f4e200","#f7db00","#fbd500","#fece00","#ffc400","#ffba00",
  "#ffb000","#ffa500","#ff9b00","#ff8000","#ff6100","#ff4200","#ff2400","#ff0500","#f90000","#f20000","#eb0000",
  "#e50000","#de0000","#da0000","#d60000","#d30000","#d00000","#cc0000","#cc2727","#cc5050","#cc7a7a","#cca3a3")




# ---------------------------------------------------------------------------
# 1. Map of Global Fishing Effort (log fighing hours/mmsi_count)
# ---------------------------------------------------------------------------

fdat <- as.data.frame(read_csv("data/total_fishing_effort.csv"))
fdat$lat_lon <- paste0(fdat$lat, "_", fdat$lon)

fdat$fishing_effort = fdat$fishing_hours/fdat$mmsi_count

ggplot() +
  geom_sf(data = world_shp, fill = 'black', color = 'black',  size = 0.1)  +
  geom_tile(data = fdat, aes(lon, lat, fill=log(fishing_effort)), alpha = 0.80) + 
  # geom_sf(data = eezs, color = 'grey', alpha = 0.5, fill = NA, size = 0.2) +
  # geom_sf(data = mpas, color = 'grey', alpha = 0.05, fill = NA, size = 0.1) +
  scale_fill_gradientn(colors = nipy_spectral[c(2:9, 9, 9, 9)], limits = c(0, 3.5), breaks = seq(0, 3.5, 0.5)) +
  theme_minimal() +
  labs(x=NULL, y=NULL, fill=NULL, title="Total Fishing Effort per vessel") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.title.align=0.5,
        panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
  coord_sf(xlim = c(0, 360), ylim = c(-80, 80), expand = FALSE) +
  guides(fill = guide_colorbar(title.position = "top",
                               direction = "vertical",
                               frame.colour = "black",
                               barwidth = .5,
                               barheight = 15)) +
  NULL

ggsave("~/Projects/World-fishing-rich-diver-equit/figures/1-World-Fishing_effort.png", width = 8, height = 4)
#






# ---------------------------------------------------------------------------
# 2. Maps of Global Fishing Effort for Chinese, US, Spanish, TWN, and Japanese
# ---------------------------------------------------------------------------

fdat <- as.data.frame(read_csv("data/total_fishing_effort_nation.csv"))

fdat$lat_lon <- paste0(fdat$lat, "_", fdat$lon)

fdat1 <- filter(fdat, flag_gfw %in% c("USA", "CHN", "JPN", "ESP"))
fdat1$flag_gfw <- ifelse(fdat1$flag_gfw == "USA", "United States", fdat1$flag_gfw)
fdat1$flag_gfw <- ifelse(fdat1$flag_gfw == "CHN", "China", fdat1$flag_gfw)
fdat1$flag_gfw <- ifelse(fdat1$flag_gfw == "ESP", "Spain", fdat1$flag_gfw)
fdat1$flag_gfw <- ifelse(fdat1$flag_gfw == "JPN", "Japan", fdat1$flag_gfw)



ggplot() +
  geom_sf(data = world_shp, fill = 'black', color = 'black',  size = 0.1)  +
  geom_tile(data = fdat1, aes(lon, lat, fill=log(1 + fishing_hours)), alpha = 0.8) + 
  geom_sf(data = eezs, color = 'grey', alpha = 0.5, fill = NA, size = 0.2) +
  geom_sf(data = mpas, color = 'grey', alpha = 0.05, fill = NA, size = 0.1) +
  scale_fill_gradientn(colors = nipy_spectral[c(0:9, 9)], limits = c(0, 11), breaks = seq(0, 11, 1)) +
  theme_minimal() +
  labs(x=NULL, y=NULL, fill=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.title.align=0.5,
        panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
  coord_sf(xlim = c(0, 360), ylim = c(-80, 80), expand = FALSE) +
  guides(fill = guide_colorbar(title.position = "top", 
                               direction = "vertical",
                               frame.colour = "black",
                               barwidth = .5,
                               barheight = 17)) +
  facet_wrap(~flag_gfw) +
  NULL

ggsave("~/Projects/World-fishing-rich-diver-equit/figures/2-Fishing_effort_CHN_ESP_JPN_USA.png", width = 8, height = 4)
#






# ---------------------------------------------------------------------------
# 3. Spatial overlap maps for CHN, USA, ESP, JPN, TWN
# ---------------------------------------------------------------------------
fdat2 <- as.data.frame(read_csv("data/total_fishing_effort_nation.csv"))
flags_ <- c("CHN", "JPN", "ESP", "TWN")

fdat2$lat_lon <- paste0(fdat2$lat, "_", fdat2$lon)
fdat2 <- filter(fdat2, fishing_hours > 0)

for (i in 1:length(flags_)){
  flag_ = flags_[i]
  mdat <- filter(fdat2, flag_gfw == flag_ | flag_gfw == "USA") 
  mdat <- mdat %>% group_by(lat_lon) %>% mutate(nn = n())
  mdat$flag_overlap <- ifelse(mdat$nn == 2, paste0("USA-", flag_), mdat$flag_gfw)
  mdat$flag_overlap <- factor(mdat$flag_overlap, levels = c("USA", flag_, paste0("USA-", flag_)))
  mdat <- as.data.frame(mdat)
  
  
p1 <- ggplot() +
  geom_sf(data = world_shp, fill = 'black', color = 'black',  size = 0.1)  +
  geom_tile(data = mdat, aes(lon, lat, fill=factor(flag_overlap)), alpha = 0.8) +
  geom_sf(data = eezs, color = 'grey', alpha = 0.5, fill = NA, size = 0.2) +
  geom_sf(data = mpas, color = 'grey', alpha = 0.05, fill = NA, size = 0.1) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(x=NULL, y=NULL,
     title=paste0("USA-", flag_, " Overlap"), fill=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        # legend.position = c(0.5, 0.12),
        legend.position = "none",
        legend.text=element_text(size=5, color='black'),
        legend.key.size = unit(.5, "cm"),
        legend.title.align=0.5,
        legend.background = element_rect(color="black"),
        panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(colour = "grey", fill=NA, size=.5)) +
  coord_sf(xlim = c(0, 360), ylim = c(-80, 80), expand=FALSE) +
  guides(fill = guide_legend(override.aes = list(size = 0.5), direction = "horizontal")) +
  NULL

  saveRDS(p1, file = paste0("data/", flag_, "-USA.rds"))
  print(flag_)

}


p1 <- readRDS("data/CHN-USA.rds")
p2 <- readRDS("data/JPN-USA.rds")
p3 <- readRDS("data/ESP-USA.rds")
p4 <- readRDS("data/TWN-USA.rds")

plot_grid(p1, p2, p3, p4, ncol=2)

ggsave("figures/3-Spatial-Map-Overlap-USA-CHN-JPN-ESP-TWN.png", width = 8, height = 4)
 
 # ---------------------------------------------------------------------
 
 

# ---------------------------------------------------------------------------
# 4. Maps of species diversity and shannon diversity globally
# ---------------------------------------------------------------------------

fdat2s <- as.data.frame(read_csv('~/Projects/World-fishing-rich-diver-equit/data/shannon_div_equ.csv'))

fdat2s$lon <- ifelse(fdat2s$lon < 0, fdat2s$lon + 360, fdat2s$lon)
fdat2s$lat_lon <- paste0(fdat2s$lat, "_", fdat2s$lon)

fdat2s <- distinct(fdat2s)
fdat2s <- filter(fdat2s, H > 0)


ggplot(NULL) +
  geom_sf(data = world_shp, fill = 'black', color = 'black',  size = 0.1)  +
  geom_tile(data = fdat2s, aes(lon, lat, fill=H), alpha = 0.8) +
  geom_sf(data = eezs, color = 'grey', alpha = 0.5, fill = NA, size = 0.2) +
  geom_sf(data = mpas, color = 'grey', alpha = 0.05, fill = NA, size = 0.1) +
  scale_fill_gradientn(colors = nipy_spectral[c(3:9, 9, 9)], limits = c(0, 2)) +
  theme_minimal() +
  labs(x=NULL, y=NULL, 
       title="Shannon Flag Diversity", fill=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
  coord_sf(xlim = c(0, 360), ylim = c(-80, 80), expand = FALSE) +
  guides(fill = guide_colorbar(title.position = "top",
                              frame.colour = "black",
                              barwidth = .35,
                              barheight = 11,
                              label.position = 'right')) +
  NULL


ggsave("figures/4-Map-Shannon-div-continuous.png", width = 6, height = 3)




# ---------------------------------------------------------------------------
# 5. Maps of shannon diversity binned globally - zero, low, med and high div
# ---------------------------------------------------------------------------

fdat6s <- as.data.frame(read_csv('data/shannon_diversity_bins_V2.csv'))

# Shannon Diversity Cut
# Categories (3, interval[float64]): [(-0.000885, 0.499] < (0.499, 0.88] < (0.88, 2.464]]
# y          
# -0.0  28631
#  1.0   5808
#  2.0   5808
#  3.0   5808  

fdat6s %>% group_by(H) %>% summarise(nn = n())

ggplot() +
  geom_tile(data = fdat6s, aes(lon, lat, fill=factor(H)), alpha = 0.8) +
  geom_sf(data = world_shp, fill = 'black', color = 'black',  size = 0.1)  +
  geom_sf(data = eezs, color = 'grey', alpha = 0.5, fill = NA, size = 0.1, inherit.aes = FALSE) +
  geom_sf(data = mpas, color = 'grey', alpha = 0.5, fill = NA, size = 0.1, inherit.aes = FALSE) +
  theme_minimal() +
  scale_fill_viridis_d(na.value = "#440154FF", labels = c("None", "Low", "Medium", "High")) +
  labs(x=NULL, y=NULL, title="Shannon Diversity", fill=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.key = element_rect(fill = NA),
        legend.position = c(.183, 0.0575),
        legend.direction = "horizontal",
        legend.background = element_rect(fill="white"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
  coord_sf(xlim = c(0, 360), ylim = c(-80, 80), expand = FALSE) +
  NULL



ggsave("figures/5-Map-Shannon-Diversity-Binned.png", width = 9, height = 4)

# ---------------------------------------------------------------------





# ---------------------------------------------------------------------------
# 6. ESM data: mean and var SST, Chl and one other globally
# ---------------------------------------------------------------------------

fdat5 <- as.data.frame(read_csv('~/Projects/World-fishing-rich-diver-equit/data/full_gfw_cmip_dat.csv'))

fdat5 <- dplyr::select(fdat5, lat, lon, mean_tos_2000_2014, var_tos_2000_2014,
                       mean_chlos_2000_2014, var_chlos_2000_2014)

mean_sst <- ggplot() +
  geom_tile(data = fdat5, aes(lon, lat, fill=mean_tos_2000_2014), alpha = 0.8) +
  geom_sf(data = world_shp, fill = 'black', color = 'black',  size = 0.1)  +
  geom_sf(data = eezs, color = 'grey', alpha = 0.5, fill = NA, size = 0.1, inherit.aes = FALSE) +
  geom_sf(data = mpas, color = 'grey', alpha = 0.5, fill = NA, size = 0.1, inherit.aes = FALSE) +
  scale_fill_gradientn(colors = color_scheme, limits = c(-2, 31), breaks = seq(-2, 31, 3)) +
  theme_minimal() +
  labs(x=NULL, y=NULL, title="Average Sea-surface Temperature", fill=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.margin=margin(-5,-5,-5,-5),
        legend.position = "bottom") +
  coord_sf(xlim = c(0, 360), ylim = c(-80, 80), expand = FALSE) +
  guides(fill = guide_colorbar(title.position = "bottom",
                              frame.colour = "black",
                              barwidth = 19,
                              barheight = 0.4,
                              label.position = "bottom")) +
  NULL

mean_sst

var_sst <- ggplot() +
  geom_tile(data = fdat5, aes(lon, lat, fill=sqrt(var_tos_2000_2014)), alpha = 0.8) +
  geom_sf(data = world_shp, fill = 'black', color = 'black',  size = 0.1)  +
  geom_sf(data = eezs, color = 'grey', alpha = 0.5, fill = NA, size = 0.1, inherit.aes = FALSE) +
  geom_sf(data = mpas, color = 'grey', alpha = 0.5, fill = NA, size = 0.1, inherit.aes = FALSE) +
  scale_fill_gradientn(colors = nipy_spectral[c(3:8, 9, 9, 9)], limits = c(0, 8), breaks = seq(0, 10, 1)) +
  theme_minimal() +
  labs(x=NULL, y=NULL, 
       title="Standard Deviation Sea-surface Temperature", fill=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.margin=margin(-5,-5,-5,-5),
        legend.position = "bottom") +
  coord_sf(xlim = c(0, 360), ylim = c(-80, 80), expand = FALSE) +
  guides(fill = guide_colorbar(title.position = "bottom",
                              frame.colour = "black",
                              barwidth = 19,
                              barheight = 0.4,
                              label.position = "bottom")) +
  NULL

var_sst

mean_chl <- ggplot() +
  geom_tile(data = fdat5, aes(lon, lat, fill=mean_chlos_2000_2014*10000000), alpha = 0.8) +
  geom_sf(data = world_shp, fill = 'black', color = 'black',  size = 0.1)  +
  geom_sf(data = eezs, color = 'grey', alpha = 0.5, fill = NA, size = 0.1, inherit.aes = FALSE) +
  geom_sf(data = mpas, color = 'grey', alpha = 0.5, fill = NA, size = 0.1, inherit.aes = FALSE) +
  scale_fill_gradientn(colors = color_scheme[c(25:99)], limits = c(0, 20), breaks = seq(0, 20, 2)) +
  theme_bw() +
  labs(x=NULL, y=NULL, 
       title="Average Chlorophyll (mg/m3)", fill=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.margin=margin(-5,-5,-5,-5),
        legend.position = "bottom") +
  coord_sf(xlim = c(0, 360), ylim = c(-80, 80), expand = FALSE) +
  guides(fill = guide_colorbar(title.position = "bottom",
                              frame.colour = "black",
                              barwidth = 19,
                              barheight = 0.4,
                              label.position = "bottom")) +
  NULL

mean_chl

var_chl <- ggplot() +
  geom_tile(data = fdat5, aes(lon, lat, fill=sqrt(var_chlos_2000_2014*10000000)), alpha = 0.8) +
  geom_sf(data = world_shp, fill = 'black', color = 'black',  size = 0.1)  +
  geom_sf(data = eezs, color = 'grey', alpha = 0.5, fill = NA, size = 0.1, inherit.aes = FALSE) +
  geom_sf(data = mpas, color = 'grey', alpha = 0.5, fill = NA, size = 0.1, inherit.aes = FALSE) +
  scale_fill_gradientn(colors = nipy_spectral[c(3:9, 9, 9, 9)], 
                       labels = c("0", "0.0025", "0.0050", "0.0075", "0.010", "0.012"), 
                       breaks = c(0, 0.0025, 0.0050, 0.0075, 0.010, 0.012), 
                       limits = c(0, 0.0125)) +
  theme_minimal() +
  labs(x=NULL, y=NULL, 
       title="Standard Deviation Chlorophyll (mg/m3)", fill=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "bottom",
        legend.margin=margin(-5,-5,-5,-5),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
  coord_sf(xlim = c(0, 360), ylim = c(-80, 80), expand = FALSE) +
  guides(fill = guide_colorbar(title.position = "bottom",
                              frame.colour = "black",
                              barwidth = 19,
                              barheight = 0.4,
                              label.position = "bottom")) +
  NULL

var_chl

plot_grid(mean_sst, var_sst, mean_chl, var_chl, ncol=2)

ggsave("figures/6-Map-ESM-SST-CHL.png", width = 9, height = 5)

# ---------------------------------------------------------------------









# ---------------------------------------------------------------------------
# 7. Fishing Hours continuous regression predicted values now and the future
# ---------------------------------------------------------------------------
ddat <- as.data.frame(read_csv("~/Projects/World-fishing-rich-diver-equit/data/NN_fishing_effort_regression_model_results.csv"))

ddat$diff <- ddat$y_ssp585_pred_2075 - ddat$y_pred_historical

ddat <- dplyr::select(ddat, lon, lat, y_pred_historical, y_ssp126_pred_2075, y_ssp370_pred_2075, y_ssp585_pred_2075)
names(ddat)[3:6] <- c("Historical Prediction", "2075-2090 SSP126 Prediction", 
                      "2075-2090 SSP370 Prediction", "2075-2090 SSP585 Prediction")

ddat <- gather(ddat, key=var, value = value, -lon, -lat)

ddat$var <- factor(ddat$var, levels = c("Historical Prediction", "2075-2090 SSP126 Prediction", 
                      "2075-2090 SSP370 Prediction", "2075-2090 SSP585 Prediction"))


ggplot() +
  geom_sf(data = world_shp, fill = 'black', color = 'black',  size = 0.1)  +
  geom_tile(data = ddat, aes(lon, lat, fill=value), alpha = 0.8) + 
  geom_sf(data = eezs, color = 'grey', alpha = 0.5, fill = NA, size = 0.2) +
  geom_sf(data = mpas, color = 'grey', alpha = 0.05, fill = NA, size = 0.1) +
  scale_fill_viridis(na.value = "#440154FF", breaks = seq(0, 1.7, 0.2), limits = c(0, 1.7)) +
  theme_minimal(12) +
  labs(x=NULL, y=NULL, fill=NULL, title = "Fishing Effort Neural Network Regression Predictions") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.title.align=0.5,
        panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.background = element_rect(fill = "#440154FF"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  coord_sf(xlim = c(0, 360), ylim = c(-80, 80), expand = FALSE) +
  guides(fill = guide_colorbar(title.position = "top", 
                               direction = "vertical",
                               frame.colour = "black",
                               barwidth = .8,
                               barheight = 16)) +
  facet_wrap(~var, ncol=2) +
  NULL






# ---------------------------------------------------------------------------
# 11. Shannon diversity classiciation predicted values now and the future
# ---------------------------------------------------------------------------

ddat11 <- as.data.frame(read_csv("~/Projects/World-fishing-rich-diver-equit/data/RF_shannon_div_classification_model_results.csv"))

ddat11$diff <- ddat11$y_ssp585_pred_2075 - ddat11$y_pred_historical

ddat11 <- dplyr::select(ddat11, lon, lat, y_pred_historical, y_ssp126_pred_2075, y_ssp370_pred_2075, y_ssp585_pred_2075)
names(ddat11)[3:6] <- c("Historical Prediction", "2075-2090 SSP126 Prediction", 
                      "2075-2090 SSP370 Prediction", "2075-2090 SSP585 Prediction")

ddat11 <- gather(ddat11, key=var, value = value, -lon, -lat)

ddat11$var <- factor(ddat11$var, levels = c("Historical Prediction", "2075-2090 SSP126 Prediction", 
                      "2075-2090 SSP370 Prediction", "2075-2090 SSP585 Prediction"))


ddat11$value <- ifelse(ddat11$value == 0, "None", ddat11$value)
ddat11$value <- ifelse(ddat11$value == 1, "Low", ddat11$value)
ddat11$value <- ifelse(ddat11$value == 2, "Medium", ddat11$value)
ddat11$value <- ifelse(ddat11$value == 3, "High", ddat11$value)

ddat11$value <- factor(ddat11$value, levels = c("None", "Low", "Medium", "High"))

ggplot() +
  geom_sf(data = world_shp, fill = 'black', color = 'black',  size = 0.1)  +
  geom_tile(data = ddat11, aes(lon, lat, fill=factor(value)), alpha = 0.8) + 
  geom_sf(data = eezs, color = 'grey', alpha = 0.5, fill = NA, size = 0.2) +
  geom_sf(data = mpas, color = 'grey', alpha = 0.05, fill = NA, size = 0.1) +
  scale_fill_viridis_d() +
  theme_minimal(12) +
  labs(x=NULL, y=NULL, fill=NULL, title = "Shannon Diversity Multiclass Random-Forest Classification Predictions") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title.align=0.5,
        panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.background = element_rect(fill = "#440154FF"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  coord_sf(xlim = c(0, 360), ylim = c(-80, 80), expand = FALSE) +
  facet_wrap(~var, ncol=2) +
  NULL




# ---------------------------------------------------------------------------
# 8. Fishing Hours error (prediction now - obs)
# ---------------------------------------------------------------------------
ddat <- as.data.frame(read_csv("~/Projects/World-fishing-rich-diver-equit/data/NN_fishing_effort_regression_model_results.csv"))

ddat$diff <- ddat$y_true_historical - ddat$y_pred_historical

ggplot() +
  geom_sf(data = world_shp, fill = 'black', color = 'black',  size = 0.1)  +
  geom_tile(data = ddat, aes(lon, lat, fill=diff), alpha = 0.8) + 
  geom_sf(data = eezs, color = 'grey', alpha = 0.5, fill = NA, size = 0.2) +
  geom_sf(data = mpas, color = 'grey', alpha = 0.05, fill = NA, size = 0.1) +
  # scale_fill_viridis(na.value = "#440154FF", breaks = seq(0, 1.7, 0.2), limits = c(0, 1.7)) +
  # scale_fill_viridis() +
  scale_fill_gradient2(low = "#1171B0", mid = "white", high="#CA0020", midpoint=0) +
  theme_minimal(12) +
  labs(x=NULL, y=NULL, fill=NULL, title = "Fishing Effort Neural Network Model Residuals") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.title.align=0.5,
        panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.background = element_rect(fill = "#440154FF"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  coord_sf(xlim = c(0, 360), ylim = c(-80, 80), expand = FALSE) +
  guides(fill = guide_colorbar(title.position = "top", 
                               direction = "vertical",
                               frame.colour = "black",
                               barwidth = .6,
                               barheight = 14.5)) +
  NULL

ggsave("figures/8-Fishing_effort_residuals.png", width = 8, height = 4)
# ---------------------------------------------------------------------------




# ---------------------------------------------------------------------------
# 9. Fishing hours difference maps
# ---------------------------------------------------------------------------
ddat <- as.data.frame(read_csv("~/Projects/World-fishing-rich-diver-equit/data/NN_fishing_effort_regression_model_results.csv"))

ddat$diff_ssp126_2030 <- ddat$y_ssp126_pred_2030 - ddat$y_pred_historical
ddat$diff_ssp370_2030 <- ddat$y_ssp370_pred_2030 - ddat$y_pred_historical
ddat$diff_ssp585_2030 <- ddat$y_ssp585_pred_2030 - ddat$y_pred_historical

ddat$diff_ssp126_2075 <- ddat$y_ssp126_pred_2075 - ddat$y_pred_historical
ddat$diff_ssp370_2075 <- ddat$y_ssp370_pred_2075 - ddat$y_pred_historical
ddat$diff_ssp585_2075 <- ddat$y_ssp585_pred_2075 - ddat$y_pred_historical


ddat <- dplyr::select(ddat, lon, lat, diff_ssp126_2030, diff_ssp370_2030, diff_ssp585_2030,
                      diff_ssp126_2075, diff_ssp370_2075, diff_ssp585_2075)
                      
names(ddat)[3:8] <- c("2030-2045 SSP126 Prediction", "2030-2045 SSP370 Prediction", "2030-2045 SSP585 Prediction",
                      "2075-2090 SSP126 Prediction", "2075-2090 SSP370 Prediction", "2075-2090 SSP585 Prediction")

ddat <- gather(ddat, key=var, value = value, -lon, -lat)

ddat$var <- factor(ddat$var, 
                   levels = c("2030-2045 SSP126 Prediction", "2030-2045 SSP370 Prediction", "2030-2045 SSP585 Prediction",
                              "2075-2090 SSP126 Prediction", "2075-2090 SSP370 Prediction", "2075-2090 SSP585 Prediction"))


ggplot() +
  geom_sf(data = world_shp, fill = 'black', color = 'black',  size = 0.1)  +
  geom_tile(data = filter(ddat, value != 0), aes(lon, lat, fill=value), alpha = 0.8) + 
  # geom_sf(data = eezs, color = 'grey', alpha = 0.5, fill = NA, size = 0.2) +
  # geom_sf(data = mpas, color = 'grey', alpha = 0.05, fill = NA, size = 0.1) +
  # scale_fill_viridis(na.value = "#440154FF", breaks = seq(0, 1.7, 0.2), limits = c(0, 1.7)) +
  # scale_fill_viridis() +
  scale_fill_gradient2(low = "#1171B0", mid = "white", high="#CA0020", midpoint=0, breaks = seq(-1.5, 1.5, 0.5), limits = c(-1.5, 1.5)) +
  theme_minimal(15) +
  labs(x=NULL, y=NULL, fill=NULL, title = "Fishing Effort Neural Network Regression Difference Predictions") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.title.align=0.5,
        panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.background = element_rect(fill = "#440154FF"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  coord_sf(xlim = c(0, 360), ylim = c(-80, 80), expand = FALSE) +
  guides(fill = guide_colorbar(title.position = "top", 
                               direction = "vertical",
                               frame.colour = "black",
                               barwidth = .8,
                               barheight = 24)) +
  facet_wrap(~var, ncol=3) +
  NULL

ggsave("figures/9-Fishing_effort_difference_maps.png", width = 18, height = 6)
# ---------------------------------------------------------------------------

