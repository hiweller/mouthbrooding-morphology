## check for overlaps between species list and UMMZ and FMNH lists from fishnet 2
## get cichlid species list ####
get_species <- function(taxa, taxonomic_level = "genus") {
  
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
  return(species_names) 
}

cichlid_genera <- c("Heros", "Bujurquina", "Tahuantinsuyoa",
                    "Aequidens", "Apistogramma", "Satanoperca",
                    "Gymnogeophagus", "Geophagus", "Oreochromis")
cichlid_species <- get_species(cichlid_genera)

# for each species
# how many lots in each collection?
# if 0 for both, stop
# if at least 1, record:
# how many lots in each museum (ex UMMZ: 10, FMNH: 0)
# mouthbrooder (Y/N)
# eggs?
# larvae?
# habitat 
# diet

# get species data and collections info ####

# we're gonna do this in a slow but explicit way [sex tape joke]
FMNH_cichlids <- read.csv("Spreadsheets/FMNH_Cichlidae.csv")
UMMZ_cichlids <- read.csv("Spreadsheets/UMMZ_Cichlidae.csv")
mouthbrooder <- read.csv("Spreadsheets/Neotropical_cichlid_reproduction.csv")
collections <- list(FMNH_cichlids, UMMZ_cichlids)
names(collections) <- c("FMNH", "UMMZ")

# make empty spreadsheet:
cichlid_df <- data.frame("Species" = cichlid_species,
                         "UMMZ lots" = rep(NA, length(cichlid_species)),
                         "UMMZ total indiv" = rep(NA, length(cichlid_species)),
                         "UMMZ max indiv per lot" = rep(NA, length(cichlid_species)),
                         "FMNH lots" = rep(NA, length(cichlid_species)),
                         "FMNH total indiv" = rep(NA, length(cichlid_species)),
                         "FMNH max indiv per lot" = rep(NA, length(cichlid_species)),
                         "Mouthbrooder" = rep(NA, length(cichlid_species)),
                         "Eggs" = rep(NA, length(cichlid_species)),
                         "Larvae" = rep(NA, length(cichlid_species)),
                         "Range" = rep(NA, length(cichlid_species)),
                         "Diet" = rep(NA, length(cichlid_species)),
                         "FeedingType" = rep(NA, length(cichlid_species)),
                         "Rep ref" = rep(NA, length(cichlid_species)),
                         "Rep comments" = rep(NA, length(cichlid_species)))

for (i in 1:length(cichlid_species)) {
  
  # pull out species name from species list
  sp <- cichlid_df$Species[i]
  
  if (length(grep("Oreochromis", sp)) == 0) {
    # Mouthbrooder?
    cichlid_df$Mouthbrooder[i] <- mouthbrooder$Mouthbrooder.[grep(sp, mouthbrooder$sciname)] %>% 
      as.character
    
    # Reference, comments
    cichlid_df$Rep.ref[i] <- mouthbrooder$Source[grep(sp, mouthbrooder$sciname)] %>%
      as.character
    cichlid_df$Rep.comments[i] <- mouthbrooder$AddInfos[grep(sp, mouthbrooder$sciname)] %>%
      as.character
    
    # Mouthbrooding stage (eggs? larvae? both?)
    if (cichlid_df$Mouthbrooder[i] == "N") {
      
      cichlid_df$Eggs[i] <- "N"
      cichlid_df$Larvae[i] <- "N"
      
    } else if (cichlid_df$Mouthbrooder[i] == "Y") {
      
      # Eggs, larvae, both, neither?
      stage <- mouthbrooder$MouthbroodingStage[grep(sp, mouthbrooder$sciname)]  
      
      # larvophile = eggs N, larvae Y
      # ovophile = eggs Y, larvae N
      # both = eggs Y, larvae Y
      # otherwise, ? for both
      if (tolower(stage) == "larvophile") {
        
        cichlid_df$Eggs[i] <- "N"
        cichlid_df$Larvae[i] <- "Y"
        
      } else if (tolower(stage) == "ovophile") {
        
        cichlid_df$Eggs[i] <- "Y"
        cichlid_df$Larvae[i] <- "N"
        
      } else if (tolower(stage) == "both") {
        
        cichlid_df$Eggs[i] <- "Y"
        cichlid_df$Larvae[i] <- "Y"
        
      } else {
        
        cichlid_df$Eggs[i] <- "?"
        cichlid_df$Larvae[i] <- "?"
        
      }
      
  }
  
  # Get diet info: major diet component ("herbivory2") and feeding strategy
  diet_df <- rfishbase::ecology(sp, fields = c("Herbivory2", "FeedingType"))
  
  # If a species has several entries, collapse the non-NA values
  if (nrow(diet_df) > 1) {
    diet_df <- apply(diet_df, 2, function(x) paste(x[!is.na(x)], collapse = "|"))
  }
  }
  # enter into dataframe
  cichlid_df$Diet[i] <- diet_df$Herbivory2
  cichlid_df$FeedingType[i] <- diet_df$FeedingType
  
  # get habitat info: just the description
  range_df <- ecosystem(sp)
  range_df <- range_df[-is.na(range_df$Description), ]
  cichlid_df$Range[i] <- paste(range_df$Description[range_df$Status != "introduced"],
                            collapse = "|")
    
  # is it in either collection? returns a list of length 2, each element is
  # which rows of each collection match species name
  hits <- lapply(collections, function(i) grep(sp, i$ScientificName))
  
  cichlid_df$UMMZ.lots[i] <- length(hits$UMMZ)
  cichlid_df$FMNH.lots[i] <- length(hits$FMNH)
  
  # tally up the total number of individuals across lots and the most individuals in a lot
  fmnh.count <- FMNH_cichlids$IndividualCount[hits$FMNH]
  ummz.count <- UMMZ_cichlids$IndividualCount[hits$UMMZ]
  
  # total individuals
  cichlid_df$FMNH.total.indiv[i] <- sum(fmnh.count)
  cichlid_df$UMMZ.total.indiv[i] <- sum(ummz.count)
  
  # max per lot
  cichlid_df$FMNH.max.indiv.per.lot[i] <- {if (length(fmnh.count)>0) max(fmnh.count) else 0}
  cichlid_df$UMMZ.max.indiv.per.lot[i] <- {if (length(ummz.count)>0) max(ummz.count) else 0}
  
}

# filtering results ####
museum_hits <- cichlid_df[(cichlid_df$UMMZ.total.indiv + cichlid_df$FMNH.total.indiv != 0), ]

write.csv(museum_hits, "Spreadsheets/UMMZ_FMNH_neotropical_mouthbrooder_availabilities.csv")
  