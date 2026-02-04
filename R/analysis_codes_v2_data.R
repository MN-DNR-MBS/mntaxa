#' Analysis codes
#'
#' Analysis codes applied to taxa for analyses for the MN NPC version 2.0. Analysis codes completely overlapping with accepted taxon assignments were omitted.
#'
#' @format ## `analysis_codes_v2`
#' A data frame with 2,522 rows and 4 columns:
#' \describe{
#'   \item{taxon_id}{ID of taxon in MNTaxa database.}
#'   \item{taxon}{Name of taxon in MNTaxa database.}
#'   \item{acc_assignment}{Accepted taxon assignment based on lookup_mntaxa. Note that these are not as stable as taxon ID or taxon, but can provide some context.}
#'   \itemt{analysis_code}{Code that groups together taxa that should be analyzed as a single taxon.}
#' }
#' @source MN DNR Minnesota Biological Survey staff
"analysis_codes_v2"
