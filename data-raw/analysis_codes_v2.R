#### data creation ####
# https://github.com/aekendig/npc-releve/blob/main/code/data-processing/analysis_codes_2003_2005.R
# Script at URL maps analysis codes from 2003 and 2005 on to taxa IDs and names
# currently in MNTaxa (pulled through mntaxa functions on 20260204). Codes
# were propagated through accepted assignments. Codes completed overlapping with
# accepted assignments were omitted.
# v2 refers to the version of the MN NPC for which these were used.
# These codes may eventually be added to MNTaxa and then can be loaded by
# the functions in this package.

# import data
analysis_codes_v2 <- read.csv("../npc-releve/intermediate-data/analysis_codes_v2_20260204.csv")

# save data
use_data(analysis_codes_v2, overwrite = TRUE)
