# arguments for testing pieces of lookup_mntaxa()

# load mntaxa
load_all()

# arguments
taxonomy_levels = T
sources = FALSE
phys = FALSE
releve = T
strata = FALSE
origin = FALSE
common = FALSE
cvals = FALSE
exclude = FALSE
replace_sub_var = T
replace_family = T
replace_genus = T
drop_higher = T
higher_include = c(
  "Belonia",
  "Chara",
  "Lychnothamnus",
  "Nitella",
  "Nitellopsis",
  "Spirogyra",
  "Tolypella"
)
excluded_duplicates = T
clean_duplicates = FALSE
group_accepted = T
group_analysis = T


dplyr::filter(acc_lookup, taxon_id %in% releve_taxa$taxon_id) |>
  data.frame()
