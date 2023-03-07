### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2023-03-07
# About: this is a script to explore the IE data collection

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Load and Link Data ###

# Synthetic Population # 
linkage <- data.table::fread("RData/SyntheticPopulation/SPIndividuals_Census2021_USWaveL_EW_population.csv")
setkey(linkage, ZoneID)

# Subset for Geography of Interest
linkage[, ctr_code := substr(ZoneID, 1, 1)]
linkage <- linkage[ctr_code %in% definitions$geo_subset, ]

# load & subset Understanding Society
US_raw <- data.table::fread("RData/UnderstandingSociety/l_indresp.tab")
setnames(US_raw, old = paste0(definitions$US_wave,"_hidp"), new = "hidp")
setnames(US_raw, old = paste0(definitions$US_wave,"_ivfio"), new = "ivfio")
setnames(US_raw, old = paste0(definitions$US_wave,"_sex"),   new = "sex")
setnames(US_raw, old = paste0(definitions$US_wave,"_dvage"),   new = "dvage")
setnames(US_raw, old = paste0(definitions$US_wave,"_gor_dv"), new = "gor_dv")
setnames(US_raw, old = paste0(definitions$US_wave,"_sf12pcs_dv"), new = "sf12pcs_dv")
setnames(US_raw, old = paste0(definitions$US_wave,"_sf12mcs_dv"), new = "sf12mcs_dv")
US_raw <- US_raw[, c("hidp","pidp","ivfio","sex","dvage","gor_dv",
                       "sf12pcs_dv","sf12mcs_dv"), with = FALSE]

# Linkage Synthetic Population -> Understanding Society
linkage <- merge(linkage, US_raw, by.x = "pidp", by.y = "pidp", all.x = TRUE)
setkey(linkage, ZoneID)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

## Unique ID
linkage[, n.i := .I]

## Define Mental Health States 
linkage[, mental := definitions$v.n[1]]
linkage[sf12mcs_dv < 50 & sf12mcs_dv >= 40,  mental := definitions$v.n[2]]
linkage[sf12mcs_dv < 40 | is.na(sf12mcs_dv), mental := definitions$v.n[3]]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Functions ###

# Function Defining Transition Probabilities For Use Within Micro Sim 
Probs <- function(M_it, definitions) { 
  # M_it:    health state occupied by individual i at cycle t (character variable)
  
  v.p.it <- rep(NA, definitions$n.s)     # create vector of state transition probabilities
  names(v.p.it) <- definitions$v.n       # name the vector
  
  # update v.p.it with the appropriate probabilities   
  
  # transition probabilities when in healthy state H
  v.p.it[M_it == "H"]  <- c(1 - definitions$p.HS1,
                            definitions$p.HS1,
                            0)    
  
  # transition probabilities when in sick state S1
  v.p.it[M_it == "S1"] <- c(definitions$p.S1H,
                            1 - definitions$p.S1H - definitions$p.S1S2,
                            definitions$p.S1S2)  
  
  # transition probabilities when sicker state S2
  v.p.it[M_it == "S2"] <- c(definitions$p.S2H,
                            definitions$p.S2S1,
                            1 - definitions$p.S2S1 - definitions$p.S2H)  
  
  # return transition probabilities OR produce error message if they dont sum up
  ifelse(sum(v.p.it) == 1, return(v.p.it),
         print("Probabilities do not sum to 1")) 
}       



MicroSim <- function(definitions, linkage) {
  
  # initialize matrix capturing  states for all individuals at each cycle
  m.M <- matrix(nrow = dim(linkage)[1], ncol = definitions$n.t + 1, 
                dimnames = list(linkage$n.i, 
                                paste("cycle", 0:definitions$n.t, sep = " ")))  
  # add initial health state
  m.M[, 1] <- linkage$mental  
  
  # -------------------------- #
  
  for (i in 1:dim(linkage)[1]) {
    set.seed(definitions$seed + i)                  # set the seed for every individual for the random number generator
    for (t in 1:definitions$n.t) {
      v.p <- Probs(m.M[i, t], definitions)           # calculate the transition probabilities at cycle t 
      
      m.M[i, t + 1] <- sample(definitions$v.n, prob = v.p, size = 1)  # sample the next health state and store that state in matrix m.M 
      
    } # close the loop for the time points 
    if(i/100 == round(i/100,0)) {          # display the progress of the simulation
      cat('\r', paste(trunc(i/dim(linkage)[1] * 100), "% done", sep = " "))
    }
  } # close the loop for the individuals 
  
  # -------------------------- #
  
  # create from --> to matrix TS
  TS <- paste(m.M, cbind(m.M[, -1], NA), sep = "->") 
  TS <- matrix(TS, nrow = dim(linkage)[1])
  rownames(TS) <- paste("Ind",   1:dim(linkage)[1], sep = " ")   
  colnames(TS) <- paste("Cycle", 0:definitions$n.t, sep = " ")   
  
  # -------------------------- #
  
  # create trace matrix TR # 
  TR <- t(apply(m.M, 2,
                function(x) table(factor(x, levels = definitions$v.n, ordered = TRUE))))
  TR <- TR / dim(linkage)[1]                                   # create a distribution trace
  rownames(TR) <- paste("Cycle", 0:definitions$n.t, sep = " ") # name the rows 
  colnames(TR) <- definitions$v.n                              # name the columns 
  
  # -------------------------- #
  
  # explicit return #
  results <- list(m.M = m.M, TS = TS, TR = TR) 
  return(results)  
}  

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Run Simulation ###
result_simulation  <- MicroSim(definitions = definitions, linkage = linkage) 
result_simulation$m.M
result_simulation$TS
result_simulation$TR

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Assign Results ###
linkage[, mental_t0 := result_simulation$m.M[,1]]
linkage[, mental_t1 := result_simulation$m.M[,2]]
linkage[, mental_t2 := result_simulation$m.M[,3]]
linkage[, mental_t3 := result_simulation$m.M[,4]]

# logical check
stopifnot(dim(linkage[mental_t0 != mental])[1] == 0)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
