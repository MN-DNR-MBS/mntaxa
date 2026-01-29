#' Taxonomic levels of accepted taxa
#'
#' Species, genus, family, order, class, phylum, and kingdom of accepted taxa (compiled January 2026). These values may eventually be added to MNTaxa and then can be loaded by the functions in this package.
#'
#' @format ## `tax_levels`
#' A data frame with 4,053 rows and 13 columns:
#' \describe{
#'   \item{acc_taxon_id}{MNTaxa ID for accepted taxon}
#'   \item{acc_taxon}{accepted taxon name}
#'   \item{acc_ss_sl}{accepted taxon name qualifier, in applicable}
#'   \item{acc_hybrid}{accepted hybrid taxon name with symbolic "×", if applicable}
#'   \item{acc_rank}{taxonomic rank of accepted taxon}
#'   \item{acc_species}{species name of accepted taxon, if applicable}
#'   \item{acc_genus}{genus name of accepted taxon, if applicable}
#'   \item{acc_family}{family name of accepted taxon}
#'   \item{acc_order}{order name of accepted taxon}
#'   \item{acc_class}{class name of accepted taxon}
#'   \item{acc_phylum}{phylum name of accepted taxon}
#'   \item{acc_kingdom}{kingdom name of accepted taxon}
#'   \item{acc_taxonomy_source}{taxonomy source}
#' }
#' @source MNTaxa, Integrated Taxonomic Information System (ITIS), World Flora Online (WFO), and Global Biodiversity Information Facility (GBIF)
"tax_levels"
