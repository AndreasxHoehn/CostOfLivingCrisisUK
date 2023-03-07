### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2023-03-07
# About: this is a script to explore the IE data collection

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Geo Lookup # 
geo_lookup <-  data.table::fread("RData/ShapeLookUp/OA_to_Local_Authority_District_May_2021.csv")
geo_lookup <- unique(geo_lookup[, .(lsoa11cd, msoa11cd, ladcd)])
setkey(geo_lookup, lsoa11cd)

# LINKAGE: Synthetic Population -> Geo Lookup
linkage <- merge(linkage, geo_lookup, by.x = "ZoneID", by.y = "lsoa11cd", all.x = TRUE)

# make bad mental health countable 
linkage[, N := 1]
linkage[, N_bad_t0 := 0]
linkage[mental_t0 == "S2", N_bad_t0 := 1]
linkage[, N_bad_t3 := 0]
linkage[mental_t3 == "S2", N_bad_t3 := 1]

mean_mental <- linkage[, .(N        = sum(N),
                           N_bad_t0 = sum(N_bad_t0),
                           N_bad_t3 = sum(N_bad_t3)),
                     by = c("ZoneID")]
mean_mental[, P_bad_t0 := (N_bad_t0 / N) * 100]
mean_mental[, P_bad_t3 := (N_bad_t3 / N) * 100]

# source: LSOA shapefile
shapefile <- readOGR("RData/ShapeLookUp/LSOA_(Dec_2021)_Boundaries_Full_Clipped_EW_(BFC).shp")
shapefile$ctr_code <- substr(shapefile$LSOA21CD, 1, 1)
shapefile <- shapefile[shapefile$ctr_code %in% definitions$geo_subset, ]
shapefile <- fortify(shapefile, region = "LSOA21CD")

# merge results with geography shapefile
mapdata <- data.table::data.table(merge(
  shapefile,
  mean_mental,
  by.x = "id", by.y = "ZoneID", # ID refers to LSOA
  all.x = FALSE))
mapdata  <- setkey(mapdata , order)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### GENERATE MAPS: SF12 ###

# definitions of cut offs
definitions$plot_cut_low  <- round(min(mapdata$P_bad_t0),0)
definitions$plot_cut_mid  <- round(median(mapdata$P_bad_t0),0)
definitions$plot_cut_high <- round(max(mapdata$P_bad_t0),0)

# long format for plot 
mapdata_long <- rbind(mapdata[, .(id,long,lat,order,hole,piece,group,
                                  p_bad = P_bad_t0, t = "T=0")],
                      mapdata[, .(id,long,lat,order,hole,piece,group,
                                  p_bad = P_bad_t3, t = "T=3")])

# mapping
map_bad_mental <- ggplot(data = mapdata_long,
                   aes(x = long, y = lat, fill = p_bad, group = group)) + 
  geom_polygon() + 
  coord_equal() + 
  theme_void() +
  labs(fill = "prevalence of\nbad mental health\non LSOA Level") + 
  scale_fill_gradientn(colours = definitions$map_cols,
                       values = scales::rescale(c(definitions$plot_cut_low,
                                                  definitions$plot_cut_mid, 
                                                  definitions$plot_cut_high))) +
  theme(strip.text = element_text(size = 28)) + 
  facet_wrap(~ t, ncol = 2)
ggsave(plot = map_bad_mental,
       filename = "ROutput/map_bad_mental.jpeg", units = "cm",
       width =  definitions$fig_width, height = definitions$fig_height)
