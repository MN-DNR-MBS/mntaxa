#' Load MNTaxa tables
#'
#' `load_mntaxa()`loads a selection of MNTaxa tables directly from the database into the specified environment.
#'
#' @param synonymies Binary option (TRUE, FALSE) to load synonymy table.
#' @param all_taxa Binary option (TRUE, FALSE) to load all taxa table.
#' @param taxonomy_levels Binary option (TRUE, FALSE) to load tables pertaining to taxonomy levels, including rank of taxa and taxonomic parents.
#' @param sources Binary option (TRUE, FALSE) to load tables for authorities and publications.
#' @param phys Binary option (TRUE, FALSE) to load physiognomy tables.
#' @param origin Binary option (TRUE, FALSE) to load origin tables.
#' @param common Binary option (TRUE, FALSE) to load common names table.
#' @param cvals Binary option (TRUE, FALSE) to load c-value table.
#' @param exclude Binary option (TRUE, FALSE) to load tables containing information about excluding taxa from analyses.
#' @param envir The environment where tables will be loaded (default is user's global environment).
#'
#' @returns Requested tables
#' @export
#'
#' @examples
#' load_mntaxa()
load_mntaxa <- function(synonymies = TRUE,
                        all_taxa = TRUE,
                        taxonomy_levels = FALSE,
                        sources = FALSE,
                        phys = FALSE,
                        origin = FALSE,
                        common = FALSE,
                        cvals = FALSE,
                        exclude = FALSE,
                        envir = .GlobalEnv) {
  # connect to mntaxa database
  db_con <- RODBC::odbcConnect("MNTaxa")

  # check connection
  if (db_con == -1) {
    stop("Failed to connect to MNTaxa database. Ensure the ODBC DSN 'MNTaxa' is configured on this machine.")
  }

  # synonymies
  if (synonymies == TRUE) {
    assign("syns_raw", RODBC::sqlFetch(db_con, "plants.synonymies_taxa"), envir = envir)
  }

  # all taxa
  if (all_taxa == TRUE) {
    assign("taxa_raw", RODBC::sqlFetch(db_con, "plants.taxa"), envir = envir)
  }

  # taxonomy levels
  if (taxonomy_levels == TRUE) {
    assign("pars_raw", RODBC::sqlFetch(db_con, "plants.parents_taxa"), envir = envir)
    assign("rank_raw", RODBC::sqlFetch(db_con, "plants.ranks"), envir = envir)
  }

  # sources
  if (sources == TRUE) {
    assign("auth_raw", RODBC::sqlFetch(db_con, "plants.authors"), envir = envir)
    assign("pubs_raw", RODBC::sqlFetch(db_con, "plants.publications"), envir = envir)
  }

  # physiognomy
  if (phys == TRUE) {
    assign("phys_codes_raw", RODBC::sqlFetch(db_con, "plants.physiognomies"), envir = envir)
    assign("syn_phys_raw", RODBC::sqlFetch(db_con, "plants.physiognomies_synonymies"), envir = envir)
  }

  # origin
  if (origin == TRUE) {
    assign("origin_codes_raw", RODBC::sqlFetch(db_con, "plants.native_statuses"), envir = envir)
    assign("syn_or_raw", RODBC::sqlFetch(db_con, "plants.synonymies"), envir = envir)
  }

  # common names
  if (common == TRUE) {
    assign("syn_comm_raw", RODBC::sqlFetch(db_con, "plants.common_names"), envir = envir)
  }

  # c-values
  if (cvals == TRUE) {
    assign("syn_cvals_raw", RODBC::sqlFetch(db_con, "plants.conservationism_coefficients"), envir = envir)
  }

  # exclusions
  if (exclude == TRUE) {
    assign("syn_exclude_raw", RODBC::sqlFetch(db_con, "plants.excluded_reasons_synonymies"), envir = envir)
    assign("exclude_codes_raw", RODBC::sqlFetch(db_con, "plants.excluded_reasons"), envir = envir)
  }

  # close database connection
  RODBC::odbcClose(db_con)

  # invisible return value
  invisible(NULL)
}
