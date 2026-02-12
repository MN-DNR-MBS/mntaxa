#' Synonymies and taxa table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{taxon_id}{Integer. Foreign key linking to \code{taxa_raw}.}
#'   \item{synonymy_id}{Integer. Identifier for the synonymy group.}
#'   \item{d_list_beg_date}{Date. Date the taxon was added to the accepted list for this synonymy group.}
#'   \item{d_list_end_date}{Date. Date the taxon was removed from the accepted list for this synonymy group. \code{NA} indicates currently accepted.}
#' }
#' @source MNTaxa database, plants.synonymies_taxa table.
"syns_raw"


#' All taxa table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{id}{Integer. Primary key.}
#'   \item{taxon}{Character. Scientific name of the taxon.}
#'   \item{is_hybrid}{Integer. Binary indicator of whether the taxon is a hybrid (1) or not (0).}
#'   \item{hybrid_parents}{Character. Names of parent taxa for hybrids. \code{NA} for non-hybrids.}
#'   \item{rank_id}{Integer. Foreign key linking to \code{rank_raw}.}
#'   \item{author_id}{Integer. Foreign key linking to \code{auth_raw}.}
#'   \item{publication_id}{Integer. Foreign key linking to \code{pubs_raw}.}
#'   \item{begin_date}{Date. Date the taxon was added to MNTaxa.}
#'   \item{end_date}{Date. Date the taxon was removed from MNTaxa. \code{NA} if current.}
#'   \item{created_at}{POSIXct. Timestamp of record creation.}
#'   \item{updated_at}{POSIXct. Timestamp of most recent record update.}
#' }
#' @source MNTaxa database, plants.taxa table.
"taxa_raw"


#' Taxonomic parents table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{taxon_id}{Integer. Foreign key linking to \code{taxa_raw}.}
#'   \item{parent_id}{Integer. Foreign key linking to the parent taxon in \code{taxa_raw}.}
#' }
#' @source MNTaxa database, plants.parents_taxa table.
"pars_raw"


#' Taxonomic rank codes table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{id}{Integer. Primary key.}
#'   \item{rank}{Character. Name of the taxonomic rank (e.g., species, genus, family).}
#' }
#' @source MNTaxa database, plants.ranks table.
"rank_raw"


#' Author codes table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{id}{Integer. Primary key.}
#'   \item{author}{Character. Abbreviated or full name of the taxonomic authority.}
#' }
#' @source MNTaxa database, plants.authors table.
"auth_raw"


#' Publication codes table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{id}{Integer. Primary key.}
#'   \item{title}{Character. Title of the publication.}
#' }
#' @source MNTaxa database, plants.publications table.
"pubs_raw"


#' Physiognomy codes table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{id}{Integer. Primary key.}
#'   \item{physiognomy}{Character. Name of the physiognomy class (e.g., forb, shrub, tree).}
#'   \item{physiognomy_code}{Character. Abbreviated code for the physiognomy class.}
#' }
#' @source MNTaxa database, plants.physiognomies table.
"phys_codes_raw"


#' Synonymy-physiognomy crosswalk table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{synonymy_id}{Integer. Foreign key linking to \code{syns_raw}.}
#'   \item{physiognomy_id}{Integer. Foreign key linking to \code{phys_codes_raw}.}
#'   \item{is_acceptable}{Integer. Binary indicator of whether this physiognomy assignment is acceptable (1) or not (0).}
#' }
#' @source MNTaxa database, plants.physiognomies_synonymies table.
"syn_phys_raw"


#' Native status codes table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{id}{Integer. Primary key.}
#'   \item{native_status}{Character. Description of the native status (e.g., native, introduced).}
#' }
#' @source MNTaxa database, plants.native_statuses table.
"origin_codes_raw"


#' Synonymy-origin crosswalk table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{id}{Integer. Primary keyForeign key linking to synonymy_id in \code{syns_raw}.}
#'   \item{native_status_id}{Integer. Foreign key linking to \code{origin_codes_raw}.}
#' }
#' @source MNTaxa database, plants.synonymies table.
"syn_or_raw"


#' Common names table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{synonymy_id}{Integer. Foreign key linking to \code{syns_raw}.}
#'   \item{common_name}{Character. Common name associated with the synonymy group.}
#' }
#' @source MNTaxa database, plants.common_names table.
"syn_comm_raw"


#' Conservatism coefficients (C-values) table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{synonymy_id}{Integer. Foreign key linking to \code{syns_raw}.}
#'   \item{conservationism_coefficient}{Integer. C-value assigned to the taxon, ranging from 0 to 10.}
#'   \item{end_date}{Date. Date the C-value assignment ended. \code{NA} if currently active.}
#'   \item{publication_id}{Integer. Foreign key linking to \code{pubs_raw} indicating the source of the C-value assignment.}
#' }
#' @source MNTaxa database, plants.conservationism_coefficients table.
"syn_cvals_raw"


#' Synonymy-exclusion crosswalk table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{synonymy_id}{Integer. Foreign key linking to \code{syns_raw}.}
#'   \item{begin_date}{Date. Date the exclusion became active.}
#'   \item{end_date}{Date. Date the exclusion ended. \code{NA} if currently active.}
#'   \item{excluded_reason_id}{Integer. Foreign key linking to \code{exclude_codes_raw}.}
#' }
#' @source MNTaxa database, plants.excluded_reasons_synonymies table.
"syn_exclude_raw"


#' Exclusion reason codes table
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{id}{Integer. Primary key.}
#'   \item{excluded_reason_code}{Character. Abbreviated code for the exclusion reason.}
#'   \item{description}{Character. Full description of the exclusion reason.}
#' }
#' @source MNTaxa database, plants.excluded_reasons table.
"exclude_codes_raw"


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


#' Releve taxa that aren't in MNTaxa
#'
#' Dataframe of taxon_id and taxon (name) to be used to include these taxa within MNTaxa output.
#'
#' @format ## `releve_taxa`
#' A data frame with 7 rows and 3 columns:
#' \describe{
#'   \item{taxon_id}{ID that's based on releve data}
#'   \item{taxon}{taxon name}
#'   \item{syonymy_id}{ID created to include these taxa in accepted table}
#' }
#' @source MN DNR releves
"releve_taxa"


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
