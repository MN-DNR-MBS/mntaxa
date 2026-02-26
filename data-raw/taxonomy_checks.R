#### set-up ####

# load functions
load_all()

# load packages
library(dplyr)

# get lookup
lookup_all <- lookup_mntaxa(taxonomy_levels = FALSE,
                            sources = FALSE,
                            releve = FALSE,
                            phys = FALSE,
                            strata = FALSE,
                            origin = FALSE,
                            common = FALSE,
                            cvals = FALSE,
                            exclude = FALSE,
                            replace_sub_var = FALSE,
                            replace_family = FALSE,
                            replace_genus = FALSE,
                            drop_higher = FALSE,
                            higher_include = c(
                              "Belonia",
                              "Chara",
                              "Lychnothamnus",
                              "Nitella",
                              "Nitellopsis",
                              "Spirogyra",
                              "Tolypella"
                            ),
                            excluded_duplicates = TRUE,
                            clean_duplicates = FALSE,
                            group_accepted = FALSE,
                            group_analysis = FALSE)

lookup_clean <- lookup_mntaxa(taxonomy_levels = FALSE,
                            sources = FALSE,
                            releve = FALSE,
                            phys = FALSE,
                            strata = FALSE,
                            origin = FALSE,
                            common = FALSE,
                            cvals = FALSE,
                            exclude = FALSE,
                            replace_sub_var = TRUE,
                            replace_family = TRUE,
                            replace_genus = TRUE,
                            drop_higher = TRUE,
                            higher_include = c(
                              "Belonia",
                              "Chara",
                              "Lychnothamnus",
                              "Nitella",
                              "Nitellopsis",
                              "Spirogyra",
                              "Tolypella"
                            ),
                            excluded_duplicates = TRUE,
                            clean_duplicates = FALSE,
                            group_accepted = TRUE,
                            group_analysis = FALSE)


#### check taxa ####

# taxa ids
ids <- c(11676,
         11967,
         12273,
         20042,
         64605,
         64606,
         68393,
         68687,
         68692,
         68726,
         68740,
         69183,
         69294,
         69298,
         69307
)

# all synonyms
filter(lookup_all, taxon_id %in% ids) |>
  distinct(taxon_id, taxon, ss_sl, acc_taxon, acc_ss_sl) |>
  arrange(taxon_id) |>
  data.frame()

filter(lookup_clean, taxon_id %in% ids) |>
  distinct(taxon_id, taxon, ss_sl, acc_full_name) |>
  arrange(taxon_id) |>
  data.frame()
