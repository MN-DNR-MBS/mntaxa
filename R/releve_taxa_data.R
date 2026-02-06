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
