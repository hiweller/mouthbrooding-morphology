# LIBRARIES ####
library(rfishbase)
library(magrittr)

# MOUTHBROODING INFO FUNCTION ####

get_fb_info <- function(taxa, taxonomic_level = "genus") {

  # load fishbase species table
  fishbase <- rfishbase::fishbase

  # identify column that matches taxon level
  col.idx <- grep(taxonomic_level, colnames(fishbase), ignore.case = TRUE)

  # find all rows that match taxon in that column and make vector of row numbers
  species_rows <- c()
  for (i in 1:length(taxa)) {
    species_rows <- c(species_rows,
                      which(as.matrix(fishbase[ , col.idx]) %in% taxa[i]))
  }

  # pull out species names based on row numbers
  species_names <- apply(fishbase[species_rows, 2:3], 1, function(x) paste(x[1], x[2]))

  # DIET
  fooditems_info <- rfishbase::fooditems(species_names)

  # LOCALITIES
  # localities <- rfishbase::distribution(species_names)
  ecologies <- ecology(species_names,
                       fields = c("Species", "Caves", "Cave", "Cave2",
                                  "Stream", "Lakes", "Herbivory2",
                                  "FeedingType",
                                  "DietTroph","DietRemark",
                                  "FoodTroph", "FoodRemark",
                                  "SoftBottom", "HardBottom",
                                  "Coarse", "Fine", "Rocky", "Rubble",
                                  "Sand", "Mud", "Ooze", "Detritus",
                                  "Organic",
                                  "Level", "Sloping",
                                  "Vegetation", "Leaves", "Stems", "Roots", "Driftwood"))

  ecosystems <- ecosystem(species_names,
                          fields = c("Species", "Status", "Abundance",
                                     "Lifestage", "Remarks.x",
                                     "EcosystemName", "EcosystemType",
                                     "Location", "LocationMap",
                                     "RiverLength", "Area",
                                     "NrangeNS", "SrangeNS", "WrangeEW", "ErangeEW",
                                     "NorthernLat", "SouthernLat", "WesternLat", "EasternLat",
                                     "Climate", "AverageDepth", "MaxDepth",
                                     "Comments", "Remarks.y",
                                     "LatDegFill", "LatMinFill", "NorthSouthFill",
                                     "LongDegFill", "LongMinFill", "EastWestFill"))
  return(list(ecologies, ecosystems))

}

# CICHLIDS ####
cichlid_genera <- c("Heros", "Bujurquina", "Tahuantinsuyoa",
                    "Aequidens", "Apistogramma", "Satanoperca",
                    "Gymnogeophagus", "Geophagus", "Oreochromis")

# get all species for each genus off of fishbase, including:
  # DIET
  # REPRODUCTIVE INFO
  # PRESENT/ABSENT IN UMMZ AND FMNH


# APOGONIDS?
# OSPHRONEMIDS?
# OPISTOGNATHIDS?
# AFRICAN CICHLIDS?
