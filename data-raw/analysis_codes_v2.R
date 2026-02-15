#### data creation ####
# mapa analysis codes from 2003 and 2005 on to accepted taxa IDs and names
# currently in MNTaxa (pulled through mntaxa functions on 20260204).
# Codes completely overlapping with accepted assignments were omitted.
# v2 refers to the version of the MN NPC for which these were used.
# These codes may eventually be added to MNTaxa and then can be loaded by
# the functions in this package.


#### set up ####

# clear environment
rm(list = ls())

# load packages
load_all() # mntaxa functions
library(tidyverse)
library(janitor)

# import codes
codes_2003 <- read_csv("../npc-releve/data/analcode2_2003_DBF.csv")
codes_2005 <- read_csv("../npc-releve/data/analcode3_2005_DBF.csv")

# import taxa
syns <- lookup_mntaxa(
  taxonomy_levels = FALSE,
  sources = FALSE,
  phys = FALSE,
  releve = FALSE,
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
  group_analysis = FALSE
)
syns_genus <- lookup_mntaxa(
  taxonomy_levels = FALSE,
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
  group_analysis = FALSE
)
taxa <- taxa_mntaxa(
  taxonomy_levels = FALSE,
  sources = FALSE,
  releve = FALSE
)


#### extract groups ####

# 2003 groups
groups_2003 <- codes_2003 %>%
  group_by(ANALCODE) %>%
  mutate(
    n_genera = n_distinct(GENUS),
    n_species = n_distinct(SPECIES, na.rm = T)
  ) %>%
  ungroup() %>%
  filter(n_genera > 1 | n_species > 1)

# 2005 groups
groups_2005 <- codes_2005 %>%
  group_by(ANALCODE) %>%
  mutate(
    n_genera = n_distinct(GENUS),
    n_species = n_distinct(SPECIES, na.rm = T)
  ) %>%
  ungroup() %>%
  filter(n_genera > 1 | n_species > 1)


#### compare groups ####

# unique to 2003
unique_2003 <- groups_2003 %>%
  anti_join(groups_2005 %>%
    distinct(LNAME)) %>%
  select(LNAME, ANALCODE) %>%
  left_join(groups_2005 %>%
    distinct(ANALCODE) %>%
    mutate(code_2005 = 1))
# Antennaria species identified by Robert Dana as being unique were removed in 2005
# same for Lactuca
# Lemna group removed in 2005 (not in dataset at all, maybe because aquatic)
# same for Typha

# look at unique groups
filter(groups_2003, ANALCODE %in% c("LEMNA", "TYPHA")) %>%
  select(LNAME, ANALCODE)
# Lemna group not relevant because L. trisulca shouldn't be included
# need to drop T. latifolia from Typha group

filter(codes_2005, str_detect(LNAME, "Lemna"))
# use L. minor for genus-level ID

# unique to 2005
unique_2005 <- groups_2005 %>%
  anti_join(groups_2003 %>%
    distinct(LNAME)) %>%
  select(LNAME, ANALCODE) %>%
  left_join(groups_2003 %>%
    distinct(ANALCODE) %>%
    mutate(code_2003 = 1))
# most are new codes (not in email discussion notes)
# some are new species

# are any groups multiple genera?
filter(groups_2003, n_genera > 1)
filter(groups_2005, n_genera > 1)
# yes

# genera grouped with species
filter(groups_2005, n_genera == 1 & is.na(SPECIES))
# only include genus if all species of the genus are in a group


#### finalize groups ####

# use 2005 plus two aquatic groups from 2003
# only include genera in groups that combine genera for now
# create name based on all species
groups <- groups_2005 %>%
  full_join(groups_2003 %>%
    filter(ANALCODE == "TYPHA" & LNAME != "Typha latifolia")) %>%
  filter(n_genera > 1 | !is.na(SPECIES)) %>%
  group_by(ANALCODE) %>%
  mutate(analysis_name = paste(sort(unique(LNAME)), collapse = "/")) %>%
  ungroup() %>%
  transmute(
    taxon = LNAME,
    analysis_code = ANALCODE,
    analysis_name = if_else(str_detect(analysis_name, "/"),
      purrr::map_chr(
        str_split(analysis_name, "/"),
        combine_names
      ),
      analysis_name
    )
  )

sort(unique(groups$analysis_code))

# compare groups to taxonomy groups
# only include groups with multiple dlist_assignments
groups_syns <- groups %>%
  mutate(taxon = str_replace(taxon, "Typha glauca", "Typha x glauca")) %>%
  left_join(
    syns %>%
      distinct(taxon, acc_assignment),
    relationship = "many-to-many"
  ) %>%
  group_by(analysis_code) %>%
  mutate(n_dlist = n_distinct(acc_assignment, na.rm = T)) %>%
  ungroup() %>%
  filter(n_dlist > 1) %>%
  distinct()

sort(unique(groups_syns$analysis_code))

# look into more
# filter(syns, str_detect(acc_assignment, "Amelanchier")) %>%
#   distinct(acc_assignment) # not all species included in analysis group
# filter(syns, str_detect(taxon, "Taraxacum")) %>%
#   distinct(taxon, acc_assignment)
# filter(syns, taxon == "Torreyochloa pallida") %>%
#   distinct(taxon, acc_assignment)
# filter(syns, str_detect(taxon, "Puccinellia")) %>%
#   distinct(taxon, acc_assignment)
# filter(syns, str_detect(taxon, "Torreyochloa")) %>%
#   distinct(taxon, acc_assignment)
# filter(groups_2005, str_detect(LNAME, "Puccinellia")) %>%
#   distinct(LNAME, ANALCODE)
# filter(syns, str_detect(taxon, "Symphoricarpos")) %>%
#      distinct(taxon, acc_assignment)
# filter(syns, str_detect(taxon, "Typha"))
# filter(groups, str_detect(taxon, "Typha"))

# select codes that are species-specific
# find dlist assignment
# add all taxa belonging to accepted assignment
groups_spp <- groups_syns %>%
  filter((analysis_code %in% c(
    "ACERSAC2", "AMEL_CMX", "ASTEERIC", "CORACMX1",
    "DRYO_CMX", "EPIL_CM1", "EPIL_CM2", "GALI_CM1",
    "HELI_CMX", "LACT_CMX", "LYCO_CM1", "ROSA_CMX",
    "RUBU_CM1", "SCIR_CP2", "SENE_CP1", "SENE_CP2",
    "SMIL_CMX", "SORB_CMX", "SYMP_CMX", "TORRPALL",
    "TYPH_CMX", "VIOL_CM1", "VIOL_CM2", "VIOL_CM3",
    "VIOL_CM4"
  ) |
    (str_detect(acc_assignment, "Oxalis") &
      str_detect(acc_assignment, "violacea") == F)) &
    !is.na(acc_assignment)) %>% # applies to taxa no longer in MNTaxa
  distinct(analysis_code, acc_assignment) %>%
  left_join(syns %>%
    select(taxon_id, taxon, acc_assignment)) %>%
  mutate(analysis_code_old = analysis_code) %>%
  group_by(analysis_code_old) %>%
  mutate(analysis_code = if_else(analysis_code_old == "TORRPALL",
    paste(sort(unique(acc_assignment)),
      collapse = "/"
    ) %>%
      paste("ecological group"),
    paste(
      word(acc_assignment, 1, 1),
      "ecological group"
    )
  )) %>%
  ungroup() %>%
  group_by(analysis_code) %>%
  mutate(n_old = n_distinct(analysis_code_old)) %>%
  ungroup() %>%
  transmute(taxon_id, taxon, acc_assignment,
    analysis_code = if_else(n_old > 1,
      paste(
        analysis_code,
        str_sub(analysis_code_old, -1)
      ),
      analysis_code
    )
  )

# check codes
sort(unique(groups_spp$analysis_code))

# select for genera in groups
syns_genus2 <- syns_genus %>%
  filter(rank == "genus" &
    str_detect(
      acc_taxon,
      "Agrimonia|Bidens|Crataegus|Cuscuta|Hackelia|Impatiens|Parthenocissus|Pilea|Sonchus|Taraxacum|Wolffia"
    ))

# taxa that don't match assignment
filter(syns_genus2, taxon != acc_taxon) %>%
  distinct(taxon, acc_taxon)

# create codes based on within-genus lumping
# select all taxa with the genus in its accepted assignment
groups_genus <- syns %>%
  filter(str_detect(
    acc_assignment,
    "Agrimonia|Bidens|Crataegus|Cuscuta|Hackelia|Impatiens|Parthenocissus|Pilea|Sonchus|Taraxacum|Wolffia"
  )) %>%
  distinct(taxon_id, taxon, acc_assignment) %>%
  full_join(syns_genus2 %>%
    distinct(taxon_id, taxon, acc_taxon) %>%
    rename(acc_assignment = acc_taxon)) %>%
  mutate(analysis_code = paste(word(acc_assignment, 1, 1), "genus"))

# check codes
sort(unique(groups_genus$analysis_code))


#### genera mapped to species ####

# 2003
genera_2003 <- codes_2003 %>%
  mutate(is_genus = if_else(LNAME == GENUS, LNAME, NA_character_)) %>%
  group_by(ANALCODE) %>%
  mutate(
    n_genera = n_distinct(is_genus, na.rm = T),
    n_species = n_distinct(SPECIES, na.rm = T)
  ) %>%
  ungroup() %>%
  filter(n_genera >= 1 & n_species == 1)

# 2005 groups
genera_2005 <- codes_2005 %>%
  mutate(is_genus = if_else(LNAME == GENUS, LNAME, NA_character_)) %>%
  group_by(ANALCODE) %>%
  mutate(
    n_genera = n_distinct(is_genus, na.rm = T),
    n_species = n_distinct(SPECIES, na.rm = T)
  ) %>%
  ungroup() %>%
  filter(n_genera >= 1 & n_species == 1)

# unique to 2003
genera_unique_2003 <- genera_2003 %>%
  anti_join(genera_2005 %>%
    distinct(LNAME)) %>%
  select(LNAME, ANALCODE) %>%
  left_join(genera_2005 %>%
    distinct(ANALCODE) %>%
    mutate(code_2005 = 1))
# none

# unique to 2005
genera_unique_2005 <- genera_2005 %>%
  anti_join(genera_2003 %>%
    distinct(LNAME)) %>%
  select(LNAME, ANALCODE) %>%
  left_join(genera_2003 %>%
    distinct(ANALCODE) %>%
    mutate(code_2003 = 1))
# Lemna minor

# format genera
genera <- genera_2005 %>%
  transmute(
    taxon = LNAME,
    analysis_code = ANALCODE,
    genus = word(taxon, 1, 1),
    n_genera,
    n_species
  ) %>%
  distinct()

# check for duplicates from analysis name
get_dupes(genera, taxon)

# remove taxonomic groups (only species in genus)
# get species-level accepted assignment for the species and genus
# remove cases that are already grouped
genera_syns <- genera %>%
  filter(genus != "Acorus") %>%
  left_join(syns %>%
    distinct(taxon, acc_assignment)) %>%
  group_by(analysis_code) %>%
  mutate(
    n_acc = n_distinct(acc_assignment), # include NA in count
    acc_assignment = paste(sort(unique(acc_assignment)), # apply species code to group
      collapse = "/"
    )
  ) %>%
  ungroup() %>%
  mutate(
    acc_genus = word(acc_assignment, 1, 1),
    analysis_code = paste(acc_assignment, "and genus")
  ) %>%
  filter(n_acc > 1)

# check for multiple accepted assignments
get_dupes(genera_syns, taxon)
filter(syns, taxon == "Acorus calamus") %>%
  select(acc_assignment)
# A. calamus is introduced, but A. americanus is native, not sure which is intended, remove

# check for multiple accepted
filter(genera_syns, str_detect(acc_assignment, "/")) %>%
  distinct(acc_assignment)
# existing dlist groups

# check for groups other than genus + species
filter(genera_syns, n_genera != 1 | n_species != 1)
# none

# are any in the other lists
filter(genera_syns, taxon %in% groups_spp$taxon)
filter(genera_syns, taxon %in% groups_genus$taxon) # yes

# look at Megalodonta
filter(syns_genus, taxon == "Megalodonta") %>%
  distinct(taxon, acc_taxon) # name has changed

# genus changes
filter(genera_syns, genus != acc_genus)
# should remove these because they've changed since these groups and
# the genus might contain other widespread species

# species changes
filter(genera_syns, taxon != acc_assignment & taxon != genus) %>%
  select(taxon, acc_assignment)

# not exactly genera
filter(genera_syns, str_detect(taxon, "\\(")) %>%
  distinct(taxon, analysis_code, genus, acc_genus)
# keep, at least one is in releves

# select genera without taxonomic changes
# rename so that the ones with parentheses are in name
genera_syns2 <- genera_syns %>%
  filter(genus == acc_genus) %>%
  group_by(analysis_code) %>%
  mutate(taxa_list = paste(sort(unique(taxon)), collapse = " and ")) %>%
  ungroup() %>%
  mutate(analysis_code = if_else(str_detect(taxa_list, "\\("),
    taxa_list, analysis_code
  ))

# get all taxa that belong with the species' accepted assignment
# get taxon ids for genera
# add dlist info
genera_syns3 <- genera_syns2 %>%
  distinct(acc_assignment, analysis_code) %>%
  inner_join(syns %>%
    select(taxon_id, taxon, acc_assignment)) %>%
  full_join(genera_syns2 %>%
    filter(taxon == genus | str_detect(taxon, "\\(")) %>%
    distinct(taxon, acc_assignment, analysis_code) %>%
    left_join(syns_genus %>%
      distinct(taxon_id, taxon)))

# check for duplicates
get_dupes(genera_syns3, taxon_id) %>%
  select(taxon_id, taxon, acc_assignment, analysis_code)

# check that all are still there
n_distinct(genera_syns2$analysis_code) == n_distinct(genera_syns3$analysis_code)


#### combine all ####

# combine and add dlist assignment info
groups_fin <- groups_spp %>%
  full_join(groups_genus) %>%
  full_join(genera_syns3)

# check for duplicates
get_dupes(groups_fin, taxon_id) %>%
  distinct(taxon_id, taxon, analysis_code)
# three missing taxon IDs


#### releve taxa ####

# add analysis code
releve_code <- releve_taxa %>%
  select(taxon_id, taxon) %>%
  arrange(taxon) %>%
  mutate(
    acc_assignment = taxon,
    analysis_code = c(
      "Epilobium ecological group 1",
      "Epilobium ecological group 2",
      "Helianthus ecological group",
      "Rubus ecological group",
      "Rubus ecological group",
      "Vaccinium (Blueberry) and Vaccinium angustifolium",
      "Vaccinium (Blueberry) and Vaccinium angustifolium"
    )
  )

# add releve taxa
groups_fin2 <- groups_fin %>%
  full_join(releve_code)

# check taxa
get_dupes(groups_fin2, taxon_id) %>%
  distinct(taxon_id, taxon, acc_assignment, analysis_code)

filter(groups_fin2, taxon == "Vaccinium (Blueberry)")

filter(groups_fin2, is.na(acc_assignment))


#### save ####

# rename
analysis_codes_v2 <- groups_fin2 %>%
  as.data.frame()

# save data
use_data(analysis_codes_v2, overwrite = TRUE)
