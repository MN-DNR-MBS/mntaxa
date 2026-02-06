#### WARNING ####
# Other scripts (esp. in data-raw) depends on the existing taxa and their
# alphabetical order. If more taxa are added, those scripts need to
# be adjusted accordingly.


# hard-coded releve taxa that aren't in MNTaxa
releve_taxa <- data.frame(
  taxon_id = c(64727, 64574, 64812, 64551, 64552, 64210, 64532),
  taxon = c(
    "Epilobium ciliatum/coloratum/glandulosum",
    "Epilobium leptophyllum/palustre/strictum",
    "Helianthus giganteus s.s./grosseserratus/nuttallii",
    "Rubus alleghenisis group",
    "Rubus alleghenisis group",
    "Vaccinium (Blueberry)",
    "Vaccinium (Blueberry)"
  )) |>
  dplyr::mutate(synonymy_id = as.numeric(paste0("1000000", taxon_id)))

# save
use_data(releve_taxa, overwrite = TRUE)
