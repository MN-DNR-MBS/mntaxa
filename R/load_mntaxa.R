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
#' @param source Character string of either package (default) or database. Database requires user to set up connection on their own.
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
                        source = c("package", "database"),
                        envir = .GlobalEnv) {
  # reports invalid value
  source <- match.arg(source)

  # use package table
  if (source == "package") {
    # synonymies
    if (synonymies) {
      assign("syns_raw", syns_raw, envir = envir)
    }

    # all taxa
    if (all_taxa) {
      assign("taxa_raw", taxa_raw, envir = envir)
    }

    # taxonomy levels
    if (taxonomy_levels) {
      assign("pars_raw", pars_raw, envir = envir)
      assign("rank_raw", rank_raw, envir = envir)
    }

    # sources
    if (sources) {
      assign("auth_raw", auth_raw, envir = envir)
      assign("pubs_raw", pubs_raw, envir = envir)
    }

    # physiognomy
    if (phys) {
      assign("phys_codes_raw", phys_codes_raw, envir = envir)
      assign("syn_phys_raw", syn_phys_raw, envir = envir)
    }

    # origin
    if (origin) {
      assign("origin_codes_raw", origin_codes_raw, envir = envir)
      assign("syn_or_raw", syn_or_raw, envir = envir)
    }

    # common names
    if (common) {
      assign("syn_comm_raw", syn_comm_raw, envir = envir)
    }

    # c-values
    if (cvals) {
      assign("syn_cvals_raw", syn_cvals_raw, envir = envir)
    }

    # exclusions
    if (exclude) {
      assign("syn_exclude_raw", syn_exclude_raw, envir = envir)
      assign("exclude_codes_raw", exclude_codes_raw, envir = envir)
    }
  } else {
    # connect to mntaxa database
    db_con <- RODBC::odbcConnect("MNTaxa")

    # check connection
    if (db_con == -1) {
      stop("Failed to connect to MNTaxa database. Ensure the ODBC DSN 'MNTaxa' is configured on this machine.")
    }

    # synonymies
    if (synonymies) {
      assign("syns_raw", RODBC::sqlFetch(db_con, "plants.synonymies_taxa"), envir = envir)
    }

    # all taxa
    if (all_taxa) {
      assign("taxa_raw", RODBC::sqlFetch(db_con, "plants.taxa"), envir = envir)
    }

    # taxonomy levels
    if (taxonomy_levels) {
      assign("pars_raw", RODBC::sqlFetch(db_con, "plants.parents_taxa"), envir = envir)
      assign("rank_raw", RODBC::sqlFetch(db_con, "plants.ranks"), envir = envir)
    }

    # sources
    if (sources) {
      assign("auth_raw", RODBC::sqlFetch(db_con, "plants.authors"), envir = envir)
      assign("pubs_raw", RODBC::sqlFetch(db_con, "plants.publications"), envir = envir)
    }

    # physiognomy
    if (phys) {
      assign("phys_codes_raw", RODBC::sqlFetch(db_con, "plants.physiognomies"), envir = envir)
      assign("syn_phys_raw", RODBC::sqlFetch(db_con, "plants.physiognomies_synonymies"), envir = envir)
    }

    # origin
    if (origin) {
      assign("origin_codes_raw", RODBC::sqlFetch(db_con, "plants.native_statuses"), envir = envir)
      assign("syn_or_raw", RODBC::sqlFetch(db_con, "plants.synonymies"), envir = envir)
    }

    # common names
    if (common) {
      assign("syn_comm_raw", RODBC::sqlFetch(db_con, "plants.common_names"), envir = envir)
    }

    # c-values
    if (cvals) {
      assign("syn_cvals_raw", RODBC::sqlFetch(db_con, "plants.conservationism_coefficients"), envir = envir)
    }

    # exclusions
    if (exclude) {
      assign("syn_exclude_raw", RODBC::sqlFetch(db_con, "plants.excluded_reasons_synonymies"), envir = envir)
      assign("exclude_codes_raw", RODBC::sqlFetch(db_con, "plants.excluded_reasons"), envir = envir)
    }

    # close database connection
    RODBC::odbcClose(db_con)
  }

  # invisible return value
  invisible(NULL)
}
