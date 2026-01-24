#### data creation ####
# https://github.com/aekendig/npc-releve/blob/main/code/data-processing/manual_physcode_stratacode.R
# Script at URL combines physcodes and growforms assigned to taxa by taxonomic
# experts in the 2005 analysis and in 2026. When experts did not provide a
# physcode, the one from MNTaxa was used.
# Growforms were converted to stratacodes, which can be ground (indicating
# no stratification should occur), shrub (indicating some stratification can
# occur), or NA (unknown or tree).
# These codes may eventually be added to MNTaxa and then can be loaded by
# the functions in this package.

# import data
phys_strata <- read.csv("../npc-releve/intermediate-data/manual_physcode_stratacode_20260122.csv")

# save data
use_data(phys_strata, overwrite = TRUE)
