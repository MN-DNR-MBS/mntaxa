#' Load MNTaxa tables to the user's environment.
#'
#' @param base Binary option (TRUE, FALSE) to load base tables of taxa and accepted taxa.
#' @param taxonomy_levels Binary option (TRUE, FALSE) to load tables pertaining to taxonomy levels, including rank of taxa and taxonomic parents.
#' @param sources Binary option (TRUE, FALSE) to load tables for authorities and publications.
#' @param phys Binary option (TRUE, FALSE) to load physiognomy tables.
#' @param origin Binary option (TRUE, FALSE) to load origin tables.
#' @param common Binary option (TRUE, FALSE) to load common names table.
#' @param cvals Binary option (TRUE, FALSE) to load c-value table.
#' @param exclude Binary option (TRUE, FALSE) to load tables containing information about excluding taxa from analyses.
#' @param envir The environment where tables will be loaded (default is user's global environment).
#'
#' @returns
#' @export
#'
#' @examples
load_mntaxa <- function(base = TRUE,
                        taxonomy_levels = FALSE,
                        sources = FALSE,
                        phys = FALSE,
                        origin = FALSE,
                        common = FALSE,
                        cvals = FALSE,
                        exclude = FALSE,
                        envir = parent.frame()) {

  # connect to mntaxa database
  db_con <- RODBC::odbcConnect("MNTaxa")

  # check connection
  if (db_con == -1) {
    stop("Failed to connect to MNTaxa database. Ensure the ODBC DSN 'MNTaxa' is configured on this machine.")
  }

  # base tables
  if (base == TRUE) {
    assign("taxa", RODBC::sqlFetch(db_con, "plants.taxa"), envir = envir)
    assign("syns", RODBC::sqlFetch(db_con, "plants.synonymies_taxa"), envir = envir)
  }

  # taxonomy levels
  if (taxonomy_levels == TRUE) {
    assign("pars", RODBC::sqlFetch(db_con, "plants.parents_taxa"), envir = envir)
    assign("rank", RODBC::sqlFetch(db_con, "plants.ranks"), envir = envir)
  }

  # sources
  if (sources == TRUE) {
    assign("auth", RODBC::sqlFetch(db_con, "plants.authors"), envir = envir)
    assign("pubs", RODBC::sqlFetch(db_con, "plants.publications"), envir = envir)
  }

  # physiognomy
  if (phys == TRUE) {
    assign("phys", RODBC::sqlFetch(db_con, "plants.physiognomies"), envir = envir)
    assign("syn_phys", RODBC::sqlFetch(db_con, "plants.physiognomies_synonymies"), envir = envir)
  }

  # origin
  if (origin == TRUE) {
    assign("origin", RODBC::sqlFetch(db_con, "plants.native_statuses"), envir = envir)
    assign("syn_or", RODBC::sqlFetch(db_con, "plants.synonymies"), envir = envir)
  }

  # common names
  if (common == TRUE) {
    assign("comms", RODBC::sqlFetch(db_con, "plants.common_names"), envir = envir)
  }

  # c-values
  if (cvals == TRUE) {
    assign("cvals", RODBC::sqlFetch(db_con, "plants.conservationism_coefficients"), envir = envir)
  }

  # exclusions
  if (exclude == TRUE) {
    assign("exclude", RODBC::sqlFetch(db_con, "plants.excluded_reasons_synonymies"), envir = envir)
    assign("exclude_codes", RODBC::sqlFetch(db_con, "plants.excluded_reasons"), envir = envir)
  }

  # close database connection
  RODBC::odbcClose(db_con)

  # invisible return value
  invisible(NULL)
}
