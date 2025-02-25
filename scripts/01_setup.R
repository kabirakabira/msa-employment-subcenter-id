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

## Create needed directories and setup base shapefiles

setupDependencies <- function() {
  if(!file.exists("permanentAssets")) {
    
    dir.create("permanentAssets")
    dir.create("permanentAssets/shapefiles")
    
    ## Get shapefiles of all MSAs and Counties
    msa.list <- core_based_statistical_areas(cb = TRUE,
                                             year = 2021) %>%
      filter(LSAD == "M1") %>%
      select(GEOID, 
             NAMELSAD, 
             geometry) %>%
      `colnames<-`(c("GEOIDm", "NAMEm", "geometry"))
    
    counties.list <- counties(state = NULL,
                              cb = TRUE,
                              year = 2021) %>%
      select(GEOID, 
             NAMELSAD, 
             STUSPS, 
             geometry) %>%
      `colnames<-`(c("GEOIDc", "NAMEc", "STc", "geometry"))
    
    ## Compute intersection of counties and MSAs
    counties.msas <- st_intersection(counties.list,
                                     msa.list) %>%
      select(GEOIDm, 
             GEOIDc, 
             NAMEm, 
             NAMEc,
             STc,
             geometry)
    
    ## Write out shapefile of MSA-County intersections
    st_write(counties.msas %>%
               st_transform(crs = 4326),
             "permanentAssets/shapefiles/msaCountyIntersection.shp")
  }
}

##### Functions

grabLEHD <- function(msa.name) {
  
  ## Read MSA-County intersections
  counties.msas <- st_read("permanentAssets/shapefiles/msaCountyIntersection.shp")
  
  ## Identify counties
  counties.msas <- counties.msas %>%
    filter(
      NAMEm == msa.name
    )
  
  ## Pull tract shapefiles for each county
  msaTracts <- tracts(
    state = counties.msas$STc[1],
    county = counties.msas$NAMEc[1],
    year = 2021
  )
  
  for (i in 2:nrow(counties.msas)) {
    temp <- tracts(
      state = counties.msas$STc[i],
      county = counties.msas$NAMEc[i],
      year = 2021
    )
    msaTracts <- msaTracts %>%
      rbind(temp)
  }
  
  msaTracts <- msaTracts %>%
    select(GEOID, geometry)
  
  ## Pull LEHD Data
  state.list <- tolower(unique(counties.msas$STc))
  lehd.baseurl <- "https://lehd.ces.census.gov/data/lodes/LODES8/"
  unlink("tempdir/", recursive = T)
  Sys.sleep(2)
  dir.create("tempdir")
  
  ### O-D Data
  for (i in 1:length(state.list)) {
    download.file(paste0(lehd.baseurl,
                         state.list[i], "/",
                         "od", "/",
                         state.list[i], "_", 
                         "od", "_", 
                         "main", "_",
                         "JT00", "_",
                         "2021", ".csv.gz"),
                  destfile = paste0("tempdir/",
                                    state.list[i],
                                    "_od",
                                    "_main",
                                    ".csv.gz"))
    download.file(paste0(lehd.baseurl,
                         state.list[i], "/",
                         "od", "/",
                         state.list[i], "_", 
                         "od", "_", 
                         "aux", "_",
                         "JT00", "_",
                         "2021", ".csv.gz"),
                  destfile = paste0("tempdir/",
                                    state.list[i],
                                    "_od",
                                    "_aux",
                                    ".csv.gz"))
  }
  
  msaOD <- read.csv(paste0("tempdir/", list.files("tempdir/")[1]))
  for (i in 2:length(list.files("tempdir/"))) {
    msaOD <- msaOD %>%
      rbind(read.csv(paste0("tempdir/", list.files("tempdir/")[i])))
  }
  unlink("tempdir/*")
  
  ### WAC Data
  for (i in 1:length(state.list)) {
    download.file(paste0(lehd.baseurl,
                         state.list[i], "/",
                         "wac", "/",
                         state.list[i], "_", 
                         "wac", "_", 
                         "S000", "_",
                         "JT00", "_",
                         "2021", ".csv.gz"),
                  destfile = paste0("tempdir/",
                                    state.list[i],
                                    "_wac",
                                    ".csv.gz"))
  }
  
  msaWAC <- read.csv(paste0("tempdir/", list.files("tempdir/")[1]))
  if (length(list.files("tempdir/")) > 1) {
    for (i in 2:length(list.files("tempdir/"))) {
      msaWAC <- msaWAC %>%
        rbind(read.csv(paste0("tempdir/", list.files("tempdir/")[i])))
    }
  }
  unlink("tempdir/", recursive = TRUE)
  
  ## Clean up MSA OD and MSA WAC data and join to MSA tract shapefile
  
  ### O-D Data
  msaOD <- msaOD %>%
    mutate(w_geocode = substr(w_geocode, 1, 11),
           h_geocode = substr(h_geocode, 1, 11))
  msaOD.workEqualsHome <- msaOD %>%
    filter(w_geocode == h_geocode) %>%
    mutate(w_geocode = as.character(w_geocode),
           h_geocode = as.character(h_geocode))
  msaOD.Inflow <- msaOD %>%
    group_by(w_geocode) %>%
    summarize(Inflow = sum(S000)) %>%
    mutate(w_geocode = as.character(w_geocode))
  msaOD.Outflow <- msaOD %>%
    group_by(h_geocode) %>%
    summarize(Outflow = sum(S000)) %>%
    mutate(h_geocode = as.character(h_geocode))
  
  msaTracts <- msaTracts %>%
    left_join(msaOD.Inflow, by = c("GEOID" = "w_geocode")) %>%
    left_join(msaOD.Outflow, by = c("GEOID" = "h_geocode")) %>%
    mutate(Inflow = ifelse(is.na(Inflow), 0, Inflow),
           Outflow = ifelse(is.na(Outflow), 0, Outflow))
  
  for (i in 1:nrow(msaTracts)) {
    ua <- msaTracts$GEOID[i]
    matched.val <- msaOD.workEqualsHome$S000[match(ua, msaOD.workEqualsHome$w_geocode)]
    if (!is.na(matched.val)) {
      msaTracts$Inflow[i] <- msaTracts$Inflow[i] - matched.val
      msaTracts$Outflow[i] <- msaTracts$Outflow[i] - matched.val
    }
  }
  
  ### WAC Data
  msaWAC <- msaWAC %>%
    select(w_geocode, 9:28) %>%
    pivot_longer(
      cols = starts_with("CNS"),
      names_to = "NAICS",
      names_prefix = "CNS",
      values_to = "Jobs",
      values_drop_na = TRUE
    ) %>%
    mutate(w_geocode = substr(w_geocode, 1, 11)) %>%
    group_by(w_geocode) %>%
    filter(Jobs > 0) %>%
    summarise(NAICS = n_distinct(NAICS),
              Jobs = sum(Jobs)) %>%
    mutate(w_geocode = as.character(w_geocode)) %>%
    filter(Jobs > 0)
  
  msaTracts <- msaTracts %>%
    left_join(msaWAC, by = c("GEOID" = "w_geocode")) %>%
    mutate(NAICS = ifelse(is.na(NAICS), 0, NAICS),
           Jobs = ifelse(is.na(Jobs), 0, Jobs))
  
  ## Calculate additional columns
  
  ### Land area and job density (in hectares)
  msaTracts <- msaTracts %>%
    mutate(
      Area.HA = as.numeric(st_area(.)) * 0.0001,
      JobDensity = Jobs / Area.HA
    )
  
  ### CBD and distance from CBD (in log(km))
  msaCBD <- msaTracts %>%
    filter(Jobs == max(Jobs))
  msaTracts$DistCBD <- as.vector(st_distance(msaCBD %>% st_centroid,
                                             msaTracts %>% st_centroid) / 1000)
  msaTracts$DistCBD <- ifelse(is.infinite(msaTracts$DistCBD), 0, msaTracts$DistCBD)
  
  ### Clean up and return shapefile
  msaTracts <- msaTracts %>%
    select(
      GEOID,
      Area.HA,
      Jobs,
      JobDensity,
      Inflow,
      Outflow,
      NAICS,
      DistCBD,
      geometry
    )
  
  return(msaTracts)
}

method.CommutingFlows <- function(msaTracts,
                                  options.contiguity) {
  
  ### Calculate FC, DDI, PC for each tract
  msaTracts <- msaTracts %>%
    mutate(
      ## FC: Flow centrality, = Inflow / Outflow
      FlowCentrality = Inflow / Outflow,
      ## DDI: Directional Dominance Index, = Inflow / (average Inflow for all UAs)
      DirectionalDominance = Inflow / (mean(Inflow, na.rm = T)),
      ## PC: Productive completeness, = number of unique NAICS / (average number of unique NAICS per UA)
      ProductiveCompleteness = NAICS / (mean(NAICS, na.rm = T)) 
    ) %>%
    mutate(
      ## In FlowCentrality, we can get NaN or Inf if Outflow == 0 | Outflow == Inflow == 0
      FlowCentrality = ifelse(!is.finite(FlowCentrality), 0, FlowCentrality)
    )
  
  ### Identify Subcenters and filter
  msaTracts <- msaTracts %>%
    mutate(
      Subcenter = ifelse(
        (FlowCentrality > 1 &
           DirectionalDominance > 1 &
           ProductiveCompleteness > 1),
        1,
        0)) %>%
    filter(
      Subcenter == 1
    )
  
  ### Dissolve contiguous units
  adjacency.matrix <- st_touches(msaTracts)
  g <- graph_from_adj_list(adjacency.matrix)
  clusters <- components(g)$membership
  
  msaTracts <- msaTracts %>%
    mutate(Cluster = clusters,
           Count = 1) %>%
    group_by(Cluster) %>%
    summarize(
      Area.HA = sum(Area.HA),
      Jobs = sum(Jobs),
      JobDensity = sum(JobDensity), # recalculate later
      Count = sum(Count)
    )
  
  ### If contiguity is required, filter by above one "Count"
  if (options.contiguity == TRUE) {
    msaTracts <- msaTracts %>%
      filter(Count > 1)
  }
  
  ### Recalculate Job Density
  msaTracts <- msaTracts %>%
    mutate(JobDensity = Jobs / Area.HA)
  
  ### Return sf object
  return(msaTracts)
  
}

method.DensityPeaks <- function(msaTracts) {
  ### Compute Log of Job Density
  msaTracts <- msaTracts %>%
    mutate(lnJobDensity = log(JobDensity))
  
  ### Find the 95th percentile for job density
  msaTracts.top5percent <- msaTracts %>%
    filter(JobDensity > quantile(JobDensity, 0.95))
  
  ### Create smoothing spline, identify knots
  fit.vt <- smooth.spline(msaTracts.top5percent$DistCBD,
                          msaTracts.top5percent$lnJobDensity,
                          cv = TRUE)
  fitted.values <- predict(fit.vt, n = 1000)
  change.indices <- which(diff(sign(diff(fitted.values$y))) < 0) + 1
  knots <- fitted.values$x[change.indices]
  knot.table <- data.frame(Knot = 1:length(knots),
                           Value = round(knots, 2))
  
  ### Find closest subcenter for each knot value
  knot.table <- knot.table %>%
    mutate(
      closest_DistCBD = map_dbl(Value, 
                                ~msaTracts.top5percent$DistCBD[which.min(abs(msaTracts.top5percent$DistCBD - .x))])
    )
  
  ### Identify subcenters and filter
  msaTracts <- msaTracts %>%
    mutate(
      Subcenter = ifelse(
        DistCBD %in% knot.table$closest_DistCBD,
        1,
        0)
    ) %>%
    #### Add the CBD
    mutate(
      Subcenter = ifelse(
        DistCBD == min(DistCBD),
        1,
        Subcenter)
    ) %>%
    filter(
      Subcenter == 1
    )
  
  ### Create buffer rings
  bufferRings <- st_buffer(st_centroid(msaTracts[which(msaTracts$Jobs == max(msaTracts$Jobs)),]),
                           knot.table$Value[1] * 1000)
  for (i in 2:nrow(knot.table)) {
    temp <- st_buffer(st_centroid(msaTracts[which(msaTracts$Jobs == max(msaTracts$Jobs)),]),
                      knot.table$Value[i] * 1000)
    bufferRings <- rbind(bufferRings,
                         temp)
  }
  bufferRings <- bufferRings %>%
    mutate(ID = seq(1, nrow(bufferRings), 1)) %>%
    select(ID, geometry)
  
  ### Clean up sf object
  msaTracts <- msaTracts %>%
    mutate(Cluster = NA,
           Count = NA) %>%
    select(Cluster, 
           Area.HA, 
           Jobs, 
           JobDensity, 
           Count, 
           geometry)
  
  ### Need tracts + rings; make a list
  output.list <- list()
  output.list[[1]] <- msaTracts
  output.list[[2]] <- bufferRings
  
  ### Return list
  return(output.list)
}

method.DoubleThresholds <- function(msaTracts,
                                    options.contiguity) {
  
  ### Set thresholds for total jobs and job density
  threshold.jobs <- mean(msaTracts$Jobs, na.rm = T) + sd(msaTracts$Jobs, na.rm = T)
  threshold.jobdensity <- mean(msaTracts$JobDensity, na.rm = T)
  
  ### Identify subcenters and filter
  msaTracts <- msaTracts %>%
    mutate(
      Subcenter = ifelse(
        (Jobs > threshold.jobs &
           JobDensity > threshold.jobdensity),
        1,
        0)
    ) %>%
    filter(Subcenter == 1)
  
  ### Dissolve contiguous units
  adjacency.matrix <- st_touches(msaTracts)
  g <- graph_from_adj_list(adjacency.matrix)
  clusters <- components(g)$membership
  
  msaTracts <- msaTracts %>%
    mutate(Cluster = clusters,
           Count = 1) %>%
    group_by(Cluster) %>%
    summarize(
      Area.HA = sum(Area.HA),
      Jobs = sum(Jobs),
      JobDensity = sum(JobDensity), # recalculate later
      Count = sum(Count)
    )
  
  ### If contiguity is required, filter by above one "Count"
  if (options.contiguity == TRUE) {
    msaTracts <- msaTracts %>%
      filter(Count > 1)
  }
  
  ### Recalculate Job Density
  msaTracts <- msaTracts %>%
    mutate(JobDensity = Jobs / Area.HA)
  
  ### Return sf object
  return(msaTracts)
  
}

method.PositiveResiduals <- function(msaTracts,
                                     options.alphaLevel,
                                     options.jobCutoff) {
  
  ### Compute log of DistCBD
  msaTracts$logDistCBD <- ifelse(msaTracts$DistCBD == 0, 0, log(msaTracts$DistCBD))
  
  ### Convert to sp object and run gwr
  msaTracts.sp <- as(msaTracts, "Spatial")
  
  gwr.bw <- gwr.sel(Jobs ~ logDistCBD,
                    msaTracts.sp,
                    adapt = TRUE)
  
  gwr.fit <- gwr(Jobs ~ logDistCBD,
                 data = msaTracts.sp,
                 adapt = gwr.bw,
                 se.fit = T,
                 hatmatrix = T)
  
  gwr.results <- as.data.frame(gwr.fit$SDF)
  
  ### Join results to tracts, calculate positivity of residuals
  msaTracts <- cbind(msaTracts,
                     select(gwr.results,
                            pred,
                            pred.se))
  
  msaTracts <- msaTracts %>%
    mutate(
      pred = ifelse(pred < 0, 0, pred),
      resid = (Jobs - pred) / pred.se
    )
  
  ### Measure "significant" positivity based on supplied alphaLevel
  if (options.alphaLevel == "0.01") {
    msaTracts <- msaTracts %>%
      mutate(
        Subcenter = ifelse(
          resid > 2.576, ## critical value; two-tailed 99%
          1,
          0)
      )
  } else if (options.alphaLevel == "0.05") {
    msaTracts <- msaTracts %>%
      mutate(
        Subcenter = ifelse(
          resid > 1.960, ## critical value; two-tailed 95%
          1,
          0)
      )
  } else if (optons.alphaLevel == "0.10") {
    msaTracts <- msaTracts %>%
      mutate(
        Subcenter = ifelse(
          resid > 1.645, ## critical value; two-tailed 90%
          1,
          0)
      )
  }
  
  ### Filter
  msaTracts <- msaTracts %>%
    filter(Subcenter == 1)
  
  ### Dissolve contiguous units
  adjacency.matrix <- st_touches(msaTracts)
  g <- graph_from_adj_list(adjacency.matrix)
  clusters <- components(g)$membership
  
  msaTracts <- msaTracts %>%
    mutate(Cluster = clusters,
           Count = 1) %>%
    group_by(Cluster) %>%
    summarize(
      Area.HA = sum(Area.HA),
      Jobs = sum(Jobs),
      JobDensity = sum(JobDensity), # recalculate later
      Count = sum(Count)
    )
  
  ### Filter by supplied jobCutoff value
  msaTracts <- msaTracts %>%
    filter(Jobs >= options.jobCutoff)
  
  ### Recalculate Job Density
  msaTracts <- msaTracts %>%
    mutate(JobDensity = Jobs / Area.HA)
  
  ### Return sf object
  return(msaTracts)
  
}

method.SpatialAutocorrelation <- function(msaTracts,
                                          options.alphaLevel,
                                          options.nearestNeighbors) {
  
  ### Set up Moran's I dependencies; log of employment density
  msaTracts$lnJobDensity <- log(msaTracts$JobDensity)
  msaTracts$lnJobDensity <- ifelse(is.infinite(msaTracts$lnJobDensity),
                                   0,
                                   msaTracts$lnJobDensity)
  
  ### Create knn weighting matrix
  knn.weights <- knearneigh(st_coordinates(st_centroid(msaTracts$geometry)),
                            k = options.nearestNeighbors)
  knn.list <- nb2listw(knn2nb(knn.weights), style = "W")
  
  ### Run local Moran's I
  local.moran <- localmoran(msaTracts$lnJobDensity, knn.list)
  local.moran <- data.frame(local.moran, attr(local.moran, "quad"))
  names(local.moran)[5] <- "p_val"
  local.moran$row <- 1:dim(msaTracts)[1]
  
  ### Merge the results with the tracts
  msaTracts$row <- 1:dim(msaTracts)[1]
  msaTracts <- merge(msaTracts, local.moran,
                     by = "row",
                     all.x = TRUE,
                     sort = FALSE)
  
  ### Identify subcenters based on supplied alpha level
  if (options.alphaLevel == "0.01") {
    msaTracts <- msaTracts %>%
      mutate(
        Subcenter = ifelse(p_val < 0.01,
                           1,
                           0)
      )
  } else if (options.alphaLevel == "0.05") {
    msaTracts <- msaTracts %>%
      mutate(
        Subcenter = ifelse(p_val < 0.05,
                           1,
                           0)
      )
  } else if (options.alphaLevel == "0.10") {
    msaTracts <- msaTracts %>%
      mutate(
        Subcenter = ifelse(p_val < 0.1,
                           1,
                           0)
      )
  }
  
  ### Join "Category" information
  msaTracts <- msaTracts %>%
    filter(Subcenter == 1) %>%
    mutate(
      Category = as.character(mean)
    ) %>%
    filter(Jobs > 0) %>%
    filter(Category %in% c("High-High", "High-Low"))
  
  ### Separately dissolve the "High-High" and "High-Low" tracts, then combine
  msaTracts.highhigh <- msaTracts %>%
    filter(Category == "High-High")
  adjacency.matrix <- st_touches(msaTracts.highhigh)
  g <- graph_from_adj_list(adjacency.matrix)
  clusters <- components(g)$membership
  msaTracts.highhigh <- msaTracts.highhigh %>%
    mutate(Cluster = clusters,
           Count = 1) %>%
    group_by(Cluster) %>%
    summarize(
      Area.HA = sum(Area.HA),
      Jobs = sum(Jobs),
      JobDensity = sum(JobDensity), # recalculate later
      Category = max(Category),
      Count = sum(Count)
    )
  
  msaTracts.highlow <- msaTracts %>%
    filter(Category == "High-Low")
  adjacency.matrix <- st_touches(msaTracts.highlow)
  g <- graph_from_adj_list(adjacency.matrix)
  clusters <- components(g)$membership
  msaTracts.highlow <- msaTracts.highlow %>%
    mutate(Cluster = clusters,
           Count = 1) %>%
    group_by(Cluster) %>%
    summarize(
      Area.HA = sum(Area.HA),
      Jobs = sum(Jobs),
      JobDensity = sum(JobDensity), # recalculate later
      Category = max(Category),
      Count = sum(Count)
    )
  
  msaTracts <- rbind(msaTracts.highhigh, msaTracts.highlow)
  
  ### Recalculate Job Density
  msaTracts <- msaTracts %>%
    mutate(JobDensity = Jobs / Area.HA)
  
  ### Renumber clusters
  msaTracts$Cluster <- seq(1, nrow(msaTracts), 1)
  
  ### Return sf object
  return(msaTracts)
  
}

############################# Testing
# setupDependencies()
# test <- grabLEHD("Chicago-Naperville-Elgin, IL-IN-WI Metro Area")
# output.CF <- method.CommutingFlows(test, TRUE)
# output.DP <- method.DensityPeaks(test)
# output.DT <- method.DoubleThresholds(test, TRUE)
# output.PR <- method.PositiveResiduals(test, "0.05", 10000)
# output.SA <- method.SpatialAutocorrelation(test, "0.05", 7)



