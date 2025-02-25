#! Setting up workspace
## Modifying options
options(scipen = 999)
options(timeout = 999)
## Installing and loading required packages
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("dplyr")
usePackage("sf")
usePackage("tigris")
usePackage("tidyr")
usePackage("spdep")
usePackage("ggplot2")
usePackage("sp")
usePackage("spgwr")
usePackage("igraph")
usePackage("purrr")
usePackage("car")
usePackage("descr")
usePackage("DescTools")
usePackage("ggpubr")
usePackage("haven")
usePackage("psych")
usePackage("writexl")
usePackage("openxlsx")
usePackage("e1071")


## Functions

## Return MSA-County intersections
grabMsaCounties <- function() {
  
  ## Grab MSAs
  msaList <- core_based_statistical_areas(cb = TRUE,
                                          year = 2021) %>%
    filter(LSAD == "M1") %>%
    select(GEOID, 
           NAMELSAD, 
           geometry) %>%
    `colnames<-`(c("GEOID_MSA", "NAME_MSA", "geometry"))
  
  ## Grab Counties
  countyList <- counties(state = NULL,
                         cb = TRUE,
                         year = 2021) %>%
    select(GEOID, 
           NAMELSAD, 
           STUSPS, 
           geometry) %>%
    `colnames<-`(c("GEOID_County", "NAME_County", "ST_County", "geometry"))
  
  ## Compute intersection of counties and MSAs
  msaCounties <- st_intersection(countyList,
                                 msaList) %>%
    select(GEOID_MSA, 
           GEOID_County, 
           NAME_MSA, 
           NAME_County,
           ST_County,
           geometry) %>%
    st_transform(crs = 4326)
  
  return(msaCounties)
  
}

## Grab Census Tracts
grabMsaTracts <- function(countyList,
                          year) {
  
  msaTracts <- tracts(
    state = countyList$ST_County[1],
    county = countyList$NAME_County[1],
    year = year
  )
  
  for (i in 2:nrow(countyList)) {
    temp <- tracts(
      state = countyList$ST_County[i],
      county = countyList$NAME_County[i],
      year = year
    )
    msaTracts <- msaTracts %>%
      rbind(temp)
  }
  
  msaTracts <- msaTracts %>%
    select(GEOID, geometry)
  
  return(msaTracts)
  
}

## Return LEHD O-D Data
grabLEHD_OD <- function(countyList,
                        year) {
  
  stateList <- tolower(unique(countyList$ST_County))
  lehdBaseURL <- "https://lehd.ces.census.gov/data/lodes/LODES8/"
  year <- as.character(year)
  unlink("data/temp/*")
  
  for (i in 1:length(stateList)) {
    download.file(
      paste0(lehdBaseURL,
             stateList[i],
             "/",
             "od",
             "/",
             stateList[i],
             "_", 
             "od",
             "_", 
             "main",
             "_",
             "JT00",
             "_",
             year,
             ".csv.gz"),
      destfile = paste0("data/temp/",
                        stateList[i],
                        "_od",
                        "_main",
                        ".csv.gz")
    )
    download.file(
      paste0(lehdBaseURL,
             stateList[i],
             "/",
             "od",
             "/",
             stateList[i],
             "_", 
             "od",
             "_", 
             "aux",
             "_",
             "JT00",
             "_",
             year,
             ".csv.gz"),
      destfile = paste0("data/temp/",
                        stateList[i],
                        "_od",
                        "_aux",
                        ".csv.gz")
    )
  }
  
  msaOD <- read.csv(paste0("data/temp/", list.files("data/temp/")[1]))
  for (i in 2:length(list.files("data/temp/"))) {
    msaOD <- msaOD %>%
      rbind(read.csv(paste0("data/temp/", list.files("data/temp/")[i])))
  }
  
  unlink("data/temp/*")
  
  ## Check if trailing 0s were removed
  msaOD$w_geocode <- ifelse(nchar(msaOD$w_geocode < 15),
                            paste0(paste0(rep(0, 15-nchar(msaOD$w_geocode)), collapse = ""), msaOD$w_geocode),
                            msaOD$w_geocode)
  msaOD$h_geocode <- ifelse(nchar(msaOD$h_geocode < 15),
                            paste0(paste0(rep(0, 15-nchar(msaOD$h_geocode)), collapse = ""), msaOD$h_geocode),
                            msaOD$h_geocode)
  
  # msaOD <- msaOD %>%
  #   mutate(w_geocode = ifelse(nchar(w_geocode < 15),
  #                             paste0(paste0(rep(0, 15-nchar(w_geocode)), collapse = ""), w_geocode),
  #                             w_geocode),
  #          h_geocode = ifelse(nchar(h_geocode < 15),
  #                             paste0(paste0(rep(0, 15-nchar(h_geocode)), collapse = ""), h_geocode),
  #                             h_geocode))
  
  return(msaOD)
  
}

## Grab LEHD WAC Data
grabLEHD_WAC <- function(countyList,
                         year) {
  
  stateList <- tolower(unique(countyList$ST_County))
  lehdBaseURL <- "https://lehd.ces.census.gov/data/lodes/LODES8/"
  year <- as.character(year)
  unlink("data/temp/*")
  
  for (i in 1:length(stateList)) {
    download.file(
      paste0(lehdBaseURL,
             stateList[i],
             "/",
             "wac",
             "/",
             stateList[i],
             "_", 
             "wac",
             "_", 
             "S000",
             "_",
             "JT00",
             "_",
             "2021", 
             ".csv.gz"),
      destfile = paste0("data/temp/",
                        stateList[i],
                        "_wac",
                        ".csv.gz")
    )
  }
  
  msaWAC <- read.csv(paste0("data/temp/", list.files("data/temp/")[1]))
  if (length(list.files("data/temp/")) > 1) {
    for (i in 2:length(list.files("data/temp/"))) {
      msaWAC <- msaWAC %>%
        rbind(read.csv(paste0("data/temp/", list.files("data/temp/")[i])))
    }
  }
  
  unlink("data/temp/*")
  
  ## Check if trailing 0s were removed
  msaWAC <- msaWAC %>%
    mutate(w_geocode = ifelse(nchar(w_geocode < 15),
                              paste0(paste0(rep(0, 15-nchar(w_geocode)), collapse = ""), w_geocode),
                              w_geocode))
  
  return(msaWAC)
  
}

## Prep MSA LEHD Data
prepLEHD <- function(msaTracts,
                     msaOD,
                     msaWAC) {
  
  
  
}




#################### TESTING
step.1 <- grabMsaCounties()
step.1 <- step.1 %>% arrange(GEOID_MSA)
step.1.5 <- step.1 %>%
  filter(NAME_MSA == "Los Angeles-Long Beach-Anaheim, CA Metro Area")

step.2 <- grabMsaTracts(step.1.5,
                        2016)

step.3 <- grabLEHD_OD(step.1.5,
                      2016)
step.4 <- grabLEHD_WAC(step.1.5,
                       2016)


### Notes for next time:
# need to fix geoid leading 0 for states like cali ("06xxx") etc
  # so it looks like the LEHD data (OD and WAC) get the leading zeroes removed. so add them back in and convert to char
