### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2023-02-23
# About: this is a script to explore the IE data collection

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Create SP #1: individual-based backbone ###

# Synthetic Population # 
SP_ind_data <- data.table::fread("RData/SyntheticPopulation/SPIndividuals_Census2021_USWaveL_EW_population.csv")
setkey(SP_ind_data, ZoneID, pidp)

# Subset for Geography of Interest
SP_ind_data[, ctr_code := substr(ZoneID, 1, 1)]
SP_ind_data <- SP_ind_data[ctr_code %in% definitions$geo_subset, ]

# add individual-level US data
US_ind_data <- data.table::fread("RData/UnderstandingSociety/l_indresp.tab")
setnames(US_ind_data, old = paste0(definitions$US_wave,"_hidp"), new = "hhid")                # instead of "hidp"
setnames(US_ind_data, old = paste0(definitions$US_wave,"_sex"),   new = "sex")
setnames(US_ind_data, old = paste0(definitions$US_wave,"_dvage"),   new = "dvage")
setnames(US_ind_data, old = paste0(definitions$US_wave,"_sf12pcs_dv"), new = "sf12pcs_dv")
setnames(US_ind_data, old = paste0(definitions$US_wave,"_sf12mcs_dv"), new = "sf12mcs_dv")
US_ind_data <- US_ind_data[, c("hhid","pidp","sex","dvage","sf12pcs_dv",
                               "sf12mcs_dv"), with = FALSE]
setkey(US_ind_data, hhid)

# linkage HH and Individuals
SP1 <- merge(SP_ind_data, US_ind_data, by = "pidp")
setkey(SP1, ZoneID, pidp)

# create N
SP1[, N := 1]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Create SP #2: HH-based backbone with individuals  ###

# Synthetic Population # 
SP_hh_data <- data.table::fread("RData/SyntheticPopulation/SPHouseholds_Census2021_USWaveL_EW_population.csv")
setkey(SP_hh_data, ZoneID, hhid)

# Subset for Geography of Interest
SP_hh_data[, ctr_code := substr(ZoneID, 1, 1)]
SP_hh_data <- SP_hh_data[ctr_code %in% definitions$geo_subset, ]

# linkage HH and Individuals
SP2 <- merge(SP_hh_data, US_ind_data, by = "hhid", allow.cartesian=TRUE)
setkey(SP2, ZoneID, hhid)

# create N
SP2[, N := 1]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Compare SP1 and SP2 ###

# Load Geo Lookup and Merge with SPs # 
geo_lookup <-  data.table::fread("RData/ShapeLookUp/OA_to_Local_Authority_District_May_2021.csv")
geo_lookup <- unique(geo_lookup[, .(lsoa11cd, msoa11cd, ladcd)])
setkey(geo_lookup, lsoa11cd)
SP1 <- merge(SP1, geo_lookup, by.x = "ZoneID", by.y = "lsoa11cd", all.x = TRUE)
SP2 <- merge(SP2, geo_lookup, by.x = "ZoneID", by.y = "lsoa11cd", all.x = TRUE)


# SP1 Aggregate by MSOA # 
SP1_msoa <- SP1[, .(N_total_SP1  = sum(N),
                    mean_age_SP1 = mean(dvage),
                    mean_sf12pcs_SP1 = mean(sf12pcs_dv),
                    mean_sf12mcs_SP1 = mean(sf12mcs_dv)),
                by = c("msoa11cd")]

# SP1 Aggregate by MSOA # 
SP2_msoa <- SP2[, .(N_total_SP2  = sum(N),
                    mean_age_SP2 = mean(dvage),
                    mean_sf12pcs_SP2 = mean(sf12pcs_dv),
                    mean_sf12mcs_SP2 = mean(sf12mcs_dv)),
                by = c("msoa11cd")]

# Merge SP1 and SP2
SP_both <- merge(SP1_msoa, SP2_msoa, by = "msoa11cd")

# abs difference: SP1 - SP2
SP_both[, N_total_diff := N_total_SP1 - N_total_SP2]
SP_both[, mean_age_diff  := mean_age_SP1 - mean_age_SP2]
SP_both[, mean_sf12pcs_diff := mean_sf12pcs_SP1 - mean_sf12pcs_SP2]
SP_both[, mean_sf12mcs_diff := mean_sf12mcs_SP1 - mean_sf12mcs_SP2]

SP_diff <- SP_both[, .(msoa11cd, N_total_diff, mean_age_diff,
                       mean_sf12pcs_diff, mean_sf12mcs_diff)]

SP_diff <- data.table::melt(SP_diff,
              measure.vars = c("N_total_diff", "mean_age_diff",
                               "mean_sf12pcs_diff", "mean_sf12mcs_diff"),
              value.name = "obs")

SP_diff[, mean_diff := mean(obs), by = "variable"]

SP_diff <- SP_diff[!is.na(msoa11cd), ]

plot_SP_diference <- ggplot2::ggplot(SP_diff, aes(x=obs)) + 
  geom_histogram(color="black", fill="white") + 
  geom_vline(aes(xintercept = mean_diff), linetype="dashed") +
  labs(caption =  paste("This histogram shows an analysis of all MSOAs in England and Wales.<br>",
                        "The analysis is a comparison of different SPs using Understanding Society wave L.<br>",
                        "Values > 1 indicate that the observation from the individual-based SP was larger.<br>",
                        "Values < 1 indicate that the observation from the houshold-based SP with additionally linked informatin on individuals was larger")) +
  facet_wrap(~ variable, ncol = 2, scales = "free",) +
  theme_classic() + 
  theme(plot.caption = element_markdown(hjust = 0))
ggsave(plot = plot_SP_diference,
       filename = "ROutput/plot_SP_difference.jpeg", units = "cm",
       width =  definitions$fig_width, height = definitions$fig_height)
