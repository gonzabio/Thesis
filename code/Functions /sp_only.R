sp_only <- function(species_list){
  #parameter name is confusing - takes a list of everything, 
  #returns species. filters out subpopulations, rank, etc. 
  df<- as.data.frame(species_list)
  df[is.na(df)] <- 0
  only_species <- subset(df, (df$result.subspecies == 0) & 
                           (df$result.rank == 0) & (df$result.subpopulation == 0))
  only_species <- only_species$result.scientific_name
  return(only_species)
}