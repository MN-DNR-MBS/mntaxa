#### overview ####
# combines physcodes and growforms assigned to taxa by taxonomic
# experts in the 2005 analysis and in 2026. When experts did not provide a
# physcode, the one from MNTaxa was used.
# Growforms were converted to stratacodes, which can be ground (indicating
# no stratification should occur), shrub (indicating some stratification can
# occur), or NA (unknown or tree).
# These codes may eventually be added to MNTaxa and then can be loaded by
# the functions in this package.

#### set-up ####

# clear environment
rm(list = ls())

# load packages
load_all() # mntaxa functions
library(tidyverse)
library(janitor)

# import data
codes_ab <- read_csv("../npc-releve/intermediate-data/stratified_taxa_for_review_20250107_AB.csv")
codes_dg <- read_csv("../npc-releve/intermediate-data/stratified_taxa_for_review_20250107_DG.csv")
codes_old <- read_csv("../npc-releve/data/analcode3_2005_DBF.csv")
growform_codes <- read_csv("../npc-releve/data/Releve spp growform codes names_DW_260112.csv")


#### format data ####

# load lookup table
lookup <- lookup_mntaxa(
  taxonomy_levels = FALSE,
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
  excluded_duplicates = FALSE,
  clean_duplicates = FALSE,
  group_accepted = FALSE,
  group_analysis = FALSE
)

# load physcodes
acc_phys <- accepted_mntaxa(
  taxonomy_levels = FALSE,
  sources = FALSE,
  releve = FALSE,
  phys = TRUE,
  strata = FALSE,
  origin = FALSE,
  common = FALSE,
  cvals = FALSE,
  exclude = FALSE
)

# ab codes
codes_ab2 <- codes_ab %>%
  rename(ab = `AB suggestion`) %>%
  rowwise() %>%
  mutate(
    pre_slash = str_split(ab, "/")[[1]][1],
    post_slash = str_split(ab, "/")[[1]][2]
  ) %>%
  ungroup() %>%
  mutate(
    physcode_ab = case_when(
      str_length(ab) == 1 ~ ab,
      str_length(pre_slash) == 1 & str_length(post_slash) == 1 ~ ab,
      str_length(pre_slash) == 1 ~ pre_slash
    ),
    stratacode_ab = case_when(
      str_detect(ab, "ground|Graminoid") ~ "ground",
      str_detect(ab, "tree|Tree") ~ NA_character_,
      str_detect(ab, "shrub|Shrub") ~ "shrub"
    ),
    stratacode_ab = if_else(!is.na(growthform) & is.na(stratacode_ab),
      growthform, stratacode_ab
    ),
    dlist_taxon = str_replace(dlist_taxon, "×", "x ") %>%
      str_replace_all("x  ", "x ")
  )

# check
codes_ab2 %>%
  distinct(ab, physcode_ab, stratacode_ab) %>%
  data.frame()

# dg codes
codes_dg2 <- codes_dg %>%
  rename(
    stratacode_dg = `dg suggestions`,
    comments_dg = `dg comments`
  ) %>%
  mutate(
    stratacode_dg = case_when(
      comments_dg == "on the line but ok?" ~ "shrub",
      str_detect(dlist_taxon, "Ribes") ~ "shrub",
      !is.na(growthform) & is.na(stratacode_dg) ~ growthform,
      TRUE ~ stratacode_dg
    ),
    physcode_dg = if_else(
      comments_dg == "would make this C? not sure on making taller versions of ths psoudospecies",
      "C", NA_character_
    ),
    dlist_taxon = str_replace(dlist_taxon, "×", "x ") %>%
      str_replace_all("x  ", "x ")
  )

# check
codes_dg2 %>%
  distinct(stratacode_dg, comments_dg)

# hybrids in codes_old are missing the x
lookup2 <- lookup %>%
  rbind(lookup %>%
    filter(!is.na(hybrid)) %>%
    mutate(taxon = str_remove(taxon, " x")))

# old codes
codes_old2 <- codes_old %>%
  rename_with(tolower) %>%
  mutate(
    lname = str_replace(lname, "Agrohordeum", "x Elyhordeum"), # update
    lname = case_when(
      lname == "Asclepias purpurescens" ~ "Asclepias purpurascens", # type-o
      lname == "Crataegus pedicellata" ~ "Crataegus coccinea", # update
      lname == "Eleocharis engelmanni" ~ "Eleocharis engelmannii", # type-o
      lname == "Lycopus sherardi" ~ "Lycopus sherardii", # type-o
      lname == "Malaxis monophylla" ~ "Malaxis monophyllos", # type-o
      lname == "Poa tomentulosa" ~ "Poa tormentuosa", # type-o
      lname == "Artemsisia" ~ "Artemisia", # type-o
      TRUE ~ lname
    )
  ) %>%
  left_join(growform_codes %>%
    rename(
      growform = `GROWFORM Code`,
      growform_name = `GROWFORM Name`
    )) %>%
  left_join(
    lookup2 %>%
      transmute(
        lname = taxon,
        dlist_taxon = acc_taxon
      ) %>%
      distinct(),
    relationship = "many-to-many"
  ) %>%
  mutate(
    stratacode_old = case_when(
      growform_name %in% c("Half-shrub", "Shrub") |
        dlist_taxon %in% c("Rubus idaeus", "Salix x fragilis") ~ "shrub",
      str_detect(growform_name, "Tree") ~ NA_character_,
      TRUE ~ "ground"
    ),
    # dlist_taxon = if_else(is.na(dlist_taxon), lname, dlist_taxon),
    dlist_taxon = str_replace(dlist_taxon, " X", " x")
  )

# no dlist taxon value
filter(codes_old2, is.na(dlist_taxon)) %>%
  pull(lname) %>%
  unique()
# all remaining either don't have a match in MNTaxa or are groups
# can keep in groups in case needed for releves

# remove non-dlist (except groups)
# remove duplication due to lookup
codes_old3 <- codes_old2 %>%
  mutate(dlist_taxon = if_else(str_detect(lname, "\\(") & is.na(dlist_taxon),
    lname, dlist_taxon
  )) %>%
  filter(!is.na(dlist_taxon)) %>%
  group_by(dlist_taxon, stratacode_old) %>%
  summarize(
    physcode_old = paste(sort(unique(na.omit(physcode))),
      collapse = "/"
    ),
    .groups = "drop"
  ) %>%
  mutate(physcode_old = if_else(physcode_old == "", NA_character_,
    physcode_old
  )) %>%
  distinct(dlist_taxon, stratacode_old, physcode_old)

# see duplicates
get_dupes(codes_old3, dlist_taxon)
# resolved above with manual taxon assignment


#### compare and resolve stratacodes ####

# combine
# use stratacodes that have agreement or are the only available
codes_comb <- codes_old3 %>%
  mutate(codes_old = 1) %>%
  full_join(codes_dg2 %>%
    distinct(dlist_taxon, stratacode_dg, physcode_dg)) %>%
  full_join(codes_ab2 %>%
    distinct(dlist_taxon, stratacode_ab, physcode_ab)) %>%
  mutate(stratacode = case_when(
    stratacode_dg == stratacode_ab & !is.na(stratacode_dg) ~ stratacode_dg,
    stratacode_dg == stratacode_old & is.na(stratacode_ab) ~ stratacode_dg,
    stratacode_ab == stratacode_old & is.na(stratacode_dg) ~ stratacode_ab,
    is.na(stratacode_old) & is.na(stratacode_ab) & !is.na(stratacode_dg) ~
      stratacode_dg,
    is.na(stratacode_old) & !is.na(stratacode_ab) & is.na(stratacode_dg) ~
      stratacode_ab,
    !is.na(stratacode_old) & is.na(stratacode_ab) & is.na(stratacode_dg) ~
      stratacode_old,
    TRUE ~ NA_character_
  ))

# conflicts
codes_comb %>%
  filter(stratacode_dg != stratacode_ab)
# checked with Google image:
# keep dg when it agrees with old
# keep ab when dg doesn't agree with old (Berberis repens)

codes_comb %>%
  filter(!is.na(stratacode_ab) & stratacode_ab != stratacode_old &
    is.na(stratacode)) %>%
  select(dlist_taxon, stratacode_old, stratacode_ab, stratacode_dg)
# use ab for all except Comptonia (because species within genus can be shrub)

codes_comb %>%
  filter(!is.na(stratacode_dg) & stratacode_dg != stratacode_old &
    is.na(stratacode)) %>%
  select(dlist_taxon, stratacode_old, stratacode_dg, stratacode_ab)
# use dg

# examine name changes associated with stratacode_old
# restricted to D and E for feasibility (too many when all taxa)
codes_comb %>%
  filter(!is.na(stratacode_old) & is.na(stratacode_ab) & is.na(stratacode_dg)) %>%
  select(dlist_taxon, stratacode_old) %>%
  inner_join(codes_old2 %>%
    filter(lname != dlist_taxon & physcode %in% c("D", "E")) %>%
    distinct(lname, dlist_taxon)) %>%
  relocate(lname)
# all dlist-stratacode pairings seem appropriate

# add additional rules based on above
codes_comb2 <- codes_comb %>%
  mutate(stratacode = case_when(
    stratacode_old == stratacode_dg ~ stratacode_dg,
    stratacode_ab != stratacode_dg & is.na(stratacode_old) ~ stratacode_ab,
    dlist_taxon %in% c("Comptonia") ~ stratacode_old,
    stratacode_ab != stratacode_old & is.na(stratacode_dg) ~ stratacode_ab,
    stratacode_dg != stratacode_old & is.na(stratacode_ab) ~ stratacode_dg,
    TRUE ~ stratacode
  ))

# any needing assignment?
codes_comb2 %>%
  filter(is.na(stratacode) & (!is.na(stratacode_ab) | !is.na(stratacode_dg) |
    !is.na(stratacode_old)))
# no


#### compare and resolve physcodes ####

# duplicates in mntaxa phys
get_dupes(acc_phys, taxon)
# all forbs, due to ss/sl

# use physcodes that have agreement or are the only available
codes_comb3 <- codes_comb2 %>%
  full_join(acc_phys %>%
    transmute(
      dlist_taxon = taxon,
      physcode_mntaxa = physcode
    )) %>%
  mutate(
    physcode = case_when(
      physcode_dg == physcode_ab & !is.na(physcode_dg) ~ physcode_dg,
      physcode_dg == physcode_old & is.na(physcode_ab) ~ physcode_dg,
      physcode_ab == physcode_old & is.na(physcode_dg) ~ physcode_ab,
      is.na(physcode_old) & is.na(physcode_ab) & !is.na(physcode_dg) ~
        physcode_dg,
      is.na(physcode_old) & !is.na(physcode_ab) & is.na(physcode_dg) ~
        physcode_ab,
      !is.na(physcode_old) & is.na(physcode_ab) & is.na(physcode_dg) ~
        physcode_old,
      is.na(physcode_old) & is.na(physcode_ab) & is.na(physcode_dg) ~
        physcode_mntaxa,
      TRUE ~ NA_character_
    )
  )

# conflicts
codes_comb3 %>%
  filter(physcode_dg != physcode_ab)
# none

codes_comb3 %>%
  filter(!is.na(physcode_ab) & physcode_ab != physcode_old) %>%
  select(dlist_taxon, physcode_old, physcode_ab, physcode_dg, physcode_mntaxa)
# use ab except old for: Gaylussacia baccata (D), and see below

codes_comb3 %>%
  filter(str_detect(dlist_taxon, "Hypericum")) %>%
  select(dlist_taxon, starts_with("physcode"))
# new: B/D/H

codes_comb3 %>%
  filter(str_detect(dlist_taxon, "Larix")) %>%
  select(dlist_taxon, starts_with("physcode"))
# N must be needleleaf deciduous (not currently an available code)
# use E (ab)

codes_comb3 %>%
  filter(str_detect(dlist_taxon, "Solanum")) %>%
  select(dlist_taxon, starts_with("physcode"))
# use H (old)

codes_comb3 %>%
  filter(!is.na(physcode_dg) & physcode_dg != physcode_old &
    is.na(physcode)) %>%
  select(dlist_taxon, physcode_old, physcode_dg, physcode_ab, physcode_mntaxa)
# none

codes_comb3 %>%
  filter(!is.na(physcode_ab) & physcode_ab != physcode_mntaxa) %>%
  select(dlist_taxon, physcode_old, physcode_ab, physcode_dg, physcode_mntaxa)
# use ab except below and Rubus arcticus subsp. acaulis (mntaxa)

codes_comb3 %>%
  filter(str_detect(dlist_taxon, "Gaylussacia")) %>%
  select(dlist_taxon, starts_with("physcode"))

codes_comb3 %>%
  filter(!is.na(physcode_dg) & physcode_dg != physcode_mntaxa) %>%
  select(dlist_taxon, physcode_old, physcode_ab, physcode_dg, physcode_mntaxa)
# none

codes_comb3 %>%
  filter(!is.na(physcode_old) & physcode_old != physcode_mntaxa &
    is.na(physcode_ab)) %>%
  select(dlist_taxon, physcode_old, physcode_mntaxa, physcode) %>%
  rowwise() %>%
  filter(str_detect(physcode_mntaxa, physcode_old) == F) %>%
  ungroup() %>%
  left_join(codes_old2 %>%
    distinct(dlist_taxon, lname)) %>%
  data.frame()
# use old instead of mntaxa (current assignment) unless noted below

codes_comb3 %>%
  filter(str_detect(dlist_taxon, "Artemisia")) %>%
  select(dlist_taxon, starts_with("physcode")) %>%
  data.frame()
# new genus-level: D/H
# species are okay using old

codes_comb3 %>%
  filter(str_detect(dlist_taxon, "Clematis")) %>%
  select(dlist_taxon, starts_with("physcode")) %>%
  data.frame()
# all should be climber

codes_comb3 %>%
  filter(str_detect(dlist_taxon, "Dasiphora")) %>%
  select(dlist_taxon, starts_with("physcode")) %>%
  data.frame()
# use mntaxa

codes_comb3 %>%
  filter(str_detect(dlist_taxon, "Decodon")) %>%
  data.frame()
# shrubby -> D

codes_comb3 %>%
  filter(str_detect(dlist_taxon, "Thymus")) %>%
  data.frame()
# creeping subshrub

codes_comb3 %>%
  filter(str_detect(dlist_taxon, "Vinca")) %>%
  data.frame()
# stratacode is correct, but they don't twine or climb

# add additional rules based on above
codes_comb4 <- codes_comb3 %>%
  mutate(
    physcode = case_when(
      dlist_taxon == "Hypericum" ~ "B/D/H",
      dlist_taxon == "Artemisia" ~ "D/H",
      str_detect(dlist_taxon, "Larix") ~ "E",
      str_detect(dlist_taxon, "Clematis") ~ "C",
      str_detect(dlist_taxon, "Decodon") ~ "D",
      str_detect(dlist_taxon, "Thymus") ~ "H",
      str_detect(dlist_taxon, "Vinca") ~ "H",
      dlist_taxon %in% c("Gaylussacia baccata", "Solanum") ~ physcode_old,
      dlist_taxon %in% c(
        "Gaylussacia", "Rubus arcticus subsp. acaulis",
        "Dasiphora"
      ) ~
        physcode_mntaxa,
      physcode_ab != physcode_old & is.na(physcode_dg) ~ physcode_ab,
      physcode_ab != physcode_mntaxa & is.na(physcode_dg) ~ physcode_ab,
      TRUE ~ physcode
    ),
    stratacode = case_when(
      str_detect(dlist_taxon, "Decodon") ~ "shrub",
      str_detect(dlist_taxon, "Thymus") ~ "ground",
      TRUE ~ stratacode
    )
  )

# any needing assignment?
codes_comb4 %>%
  filter(is.na(physcode) &
    (!is.na(physcode_ab) | !is.na(physcode_dg) |
      !is.na(physcode_old) | !is.na(physcode_mntaxa)))
# none

# missing physcodes
missing_physcodes <- codes_comb4 %>%
  filter(is.na(physcode))

# fill in some manually
missing_physcodes %>%
  filter(str_count(dlist_taxon, boundary("word")) > 1) %>%
  pull(dlist_taxon)

missing_physcodes %>%
  filter(str_count(dlist_taxon, boundary("word")) == 1 &
    str_sub(dlist_taxon, -3) != "eae") %>%
  pull(dlist_taxon)

# check codes
unique(codes_comb4$stratacode)
unique(codes_comb4$physcode)

# manual physcodes (and stratacode if needed)
# finalize columns
codes_comb5 <- codes_comb4 %>%
  mutate(
    physcode = case_when(
      dlist_taxon %in% c(
        "Cynanchum",
        "Humulus lupulus var. lupulus",
        "Lathyrus sylvestris"
      ) ~ "C",
      dlist_taxon == "Acer x freemanii" ~ "D",
      dlist_taxon == "Daphne" ~ "D/E",
      dlist_taxon %in% c(
        "Azolla caroliniana",
        "Hydrocharis",
        "Marsilea quadrifolia",
        "Trapa",
        "Nymphoides"
      ) ~ "F",
      dlist_taxon == "Pennisetum" ~ "G",
      dlist_taxon %in% c(
        "Aletris",
        "Asperugo",
        "Athyrium filix-femina var. cyclosorum",
        "Echinodorus berteroi",
        "Eruca",
        "Gentiana x pallidocyanea",
        "Glandularia",
        "Lycopodium appalachianum x lucidulum",
        "Lycopodium appalachianum x selago",
        "Petunia axillaris",
        "Polystichum lonchitis",
        "Spiranthes ovalis var. erostellata"
      ) ~ "H",
      physcode == "Li" ~ "L",
      dlist_taxon %in% c(
        "Egeria",
        "Hydrilla"
      ) ~ "S",
      TRUE ~ physcode
    ),
    stratacode = case_when(
      dlist_taxon == "Daphne" ~ "shrub",
      TRUE ~ stratacode
    )
  ) %>%
  filter(!is.na(stratacode) | !is.na(physcode)) %>%
  select(dlist_taxon, stratacode, physcode) %>%
  rename(acc_taxon = dlist_taxon) %>%
  distinct()


#### add in releve taxa ####

# genera of releve taxa
releve_genera <- paste(unique(word(releve_taxa$taxon, 1)), collapse = "|")

# codes associated with releve taxa
codes_comb5 %>%
  filter(str_detect(acc_taxon, releve_genera)) %>%
  mutate(genus = word(acc_taxon, 1)) %>%
  distinct(genus, stratacode, physcode) %>%
  arrange(genus)

filter(codes_comb5, acc_taxon == "Rubus allegheniensis") # D, shrub
filter(codes_old, LNAME == "Vaccinium (Blueberry)") # D, shrub
# all others H

# format releve taxa
releve_codes <- releve_taxa %>%
  distinct(acc_taxon = taxon) %>%
  arrange(acc_taxon) %>%
  mutate(
    stratacode = c(rep(NA_character_, 3), rep("shrub", 2)),
    physcode = c("H", "H", "H", "D", "D")
  )

# add
codes_comb6 <- codes_comb5 %>%
  full_join(releve_codes)

# check
filter(codes_comb6, acc_taxon %in% releve_taxa$taxon)


#### save ####

# rename and make dataframe
phys_strata <- codes_comb6 %>%
  as.data.frame()

# save data
use_data(phys_strata, overwrite = TRUE)
