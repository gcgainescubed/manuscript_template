# use this script to set a bunch of the initialization stuff

# 1 folder paths to important stuff

# George

# George's ext drive, or else Pinyon/windows machine
if(Sys.info()["nodename"]=="AFS5DE004J3CH53"){
  if(dir.exists("D:/projects")){
    FIAdata <- "D:/projects/sae_simulation/FIAdata/SQLite_FIADB_ENTIRE"
    geodata <- "D:/projects/sae_simulation/geospatial"
    #RAP3 <- "D:/projects/sae_simulation/RAP3"
    #dontpush <- "D:/projects/sae_simulation_dont_push"
  } else {
  FIAdata <- "C:/Users/GeorgeGaines/Documents/projects/tabdata/SQLite_FIADB_ENTIRE"
  geodata <- "C:/Users/GeorgeGaines/Documents/projects/geodata"
  #RAP3 <- "..."
  #dontpush <- "C:/Users/GeorgeGaines/Documents/projects/sae_simulation_dont_push"
  }
}

# -------------------------------------------------

# Other people...