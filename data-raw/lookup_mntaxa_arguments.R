# arguments for testing pieces of lookup_mntaxa()

# load mntaxa
load_all()

# arguments
taxonomy_levels <- F
sources <- FALSE
releve <- T
phys <- FALSE
strata <- T
origin <- FALSE
common <- FALSE
cvals <- FALSE
exclude <- FALSE
replace_sub_var <- T
replace_family <- T
replace_genus <- T
drop_higher <- T
higher_include <- c(
  "Belonia",
  "Chara",
  "Lychnothamnus",
  "Nitella",
  "Nitellopsis",
  "Spirogyra",
  "Tolypella"
)
excluded_duplicates <- T
clean_duplicates <- F
group_accepted <- T
group_analysis <- T
