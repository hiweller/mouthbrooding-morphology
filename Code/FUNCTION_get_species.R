# finds all species of a given taxonomic level, case-insensitive
# ex: get_species("Cichlidae", "family")
get_species <- function(taxa, taxonomic_level = "genus") {
  
  # load fishbase species table
  fishbase <- rfishbase::fishbase
  
  # identify column that matches taxon level
  col.idx <- match(tolower(taxonomic_level), tolower(colnames(fishbase)))
  
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
