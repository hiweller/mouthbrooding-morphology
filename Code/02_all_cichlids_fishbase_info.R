# FOR ALL CICHLIDS:
# get diet, reproduction
# in a big table!
# any species with more than one entry for a category -> concatenate unique values into a single string
# manually confirm mouthbrooding and sifting
# get a location (Central America, South America, Africa - lakes, Africa - rivers)

# for each species


# CATEGORIES:
  # not sifting, not mouthbrooding
  # sifting, not mouthbrooding
  # not sifting, mouthbrooding
  # sifting, mouthbrooding
  # incomplete but of interest: i.e. definitely mouthbrooding, feeding ambiguous or definitely sifting, reproduction ambiguous

# GENERA:
  # ID genera that fit at least two of these categories 
    # i.e. one species mouthbroods but doesn't sift, another does both

## check for overlaps between species list and UMMZ and FMNH lists from fishnet 2
## get cichlid species list ####

# get a list of all cichlid species on fishbase
source("Code/FUNCTION_get_species.R")
cichlid_species <- get_species("Cichlidae", taxonomic_level = "family")

# generate an NA dataframe with relevant columns:
cichlid_df <- data.frame("Species" = cichlid_species,
                         "Reproduction" = rep(NA, length(cichlid_species)),
                         "Reproduction.comments" = rep(NA, length(cichlid_species)),
                         "Mouthbrooder" = rep(NA, length(cichlid_species)),
                         "Diet" = rep(NA, length(cichlid_species)),
                         "FeedingType" = rep(NA, length(cichlid_species)),
                         "Sifter" = rep(NA, length(cichlid_species)),
                         "Location" = rep(NA, length(cichlid_species)),
                         "Reproduction.ref" = rep(NA, length(cichlid_species)),
                         "Eggs" = rep(NA, length(cichlid_species)),
                         "Larvae" = rep(NA, length(cichlid_species)))
for (i in 1:length(cichlid_species)) {
  
  sp <- cichlid_species[i]
  
  # Reproduction: reproductive guild, comments
  reproduction.df <- rfishbase::reproduction(sp, fields = c("RepGuild2", "AddInfos"))
  if (nrow(reproduction.df) > 1) {
    reproduction.df <- apply(reproduction.df, 2, function(x) paste(unique(x[!is.na(x)]), collapse = "|"))
  }
  
  # Diet
  # Get diet info: major diet component ("herbivory2") and feeding strategy
  diet.df <- rfishbase::ecology(sp, fields = c("Herbivory2", "FeedingType"))
  
  # If a species has several entries, collapse the non-NA values
  if (nrow(diet.df) > 1) {
    diet.df <- apply(diet.df, 2, function(x) paste(unique(x[!is.na(x)]), collapse = "|"))
  }
  
  
  # location (continent)
  fao.df <- rfishbase::faoareas(sp, fields = c("FAO", "Status"))
  fao.df <- unique(fao.df$FAO[grep("endemic|native", fao.df$Status)])
  location <- paste(fao.df, collapse = "|")
  
  # if "Africa-Inland Waters" is one of the entries, narrow down to lakes/rivers:
  if (length(grep("Africa", fao.df)) > 0) {
    ecosystem.df <- rfishbase::ecosystem(sp)
    location <- "Africa - "
    ecosystem.df <- unique(ecosystem.df[grep("endemic|native", ecosystem.df$Status),
                                        19:20])
    if(length(grep("Victoria|Tanganyika|Malawi", ecosystem.df$EcosystemName)) > 0) {
      location <- paste(location, "LAKES:", ecosystem.df$EcosystemName[grep("Victoria|Tanganyika|Malawi", ecosystem.df$EcosystemName)], collapse = "|")
    }
    
    if (length(grep("River", ecosystem.df$EcosystemType)) > 0) {
      location <- paste(location, "RIVERS", sep = " | ")
    }
  }
  
  # clumsily enter into the appropriate column:
  cichlid_df$Reproduction[i] <- reproduction.df[1]
  cichlid_df$Reproduction.comments[i] <- reproduction.df[2]
  cichlid_df$Diet[i] <- diet.df[1]
  cichlid_df$FeedingType[i] <- diet.df[2]
  cichlid_df$Location[i] <- location
  
}

# some variables save as list types - unlist them:
cichlid_OUT <- apply(cichlid_df, 2, unlist)

# get rid of any row where reproduction, diet, and feeding type are ALL NA:
na.idx <- which(apply(is.na(cichlid_OUT[, c(2, 5, 6)]), 1, function(i) sum(i) == 3))
cichlid_OUT <- cichlid_OUT[-na.idx, ]

write.csv(cichlid_OUT, "Spreadsheets/ALL_cichlids_diet_reproduction.csv")


# read in manually-edited version
cichlids_2 <- read.csv("Spreadsheets/ALL_cichlids_diet_reproduction.csv")
cichlids_2$Comments <- rep(NA, nrow(cichlids_2))

# look up comments and add extra column
for (i in 1:nrow(cichlids_2)) {
  
  cichlids_2$Comments[i] <- suppressWarnings(rfishbase::species(cichlids_2[i, 1])$Comments)
  
}

cichlids_2$Comments <- unlist(cichlids_2$Comments)

write.csv(cichlids_2, "Spreadsheets/ALL_cichlids_diet_reproduction.csv")


