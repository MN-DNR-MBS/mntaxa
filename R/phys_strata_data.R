#' Physcode and stratacode data
#'
#' Physcodes and stratacodes for accepted taxa, when available (as of January 2026). These codes may eventually be added to MNTaxa and then can be loaded by the functions in this package.
#'
#' @format ## `phys_strata`
#' A data frame with 3,914 rows and 3 columns:
#' \describe{
#'   \item{acc_taxon}{accepted taxon name (3,914 unique values)}
#'   \item{stratcode}{stratification code, one of: ground (no stratification recommended), shrub (potential stratification into groundlayer and sub-canopy recommended), NA (missing value or tree)}
#'   \item{physcode}{physiognomy code, one of: A (aquatic), B (broadleaf evergreen), C (climber), D (broadleaf deciduous), E (needleleaf evergreen), F (floating aquatic), G (graminoid), H (forb), K (stem succulent), L (lichen), S (submerged aquatic), X (epiphyte), or a combination of two or more codes separated by "/"}
#' }
#' @source MN DNR Minnesota Biological Survey staff
"phys_strata"
