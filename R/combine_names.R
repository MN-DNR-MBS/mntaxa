#' Function to combine names in a character string
#'
#' @param x A vector of taxon names.
#'
#' @returns A character string. If all names in the string are the same genus, the genus name will appear once at the beginning. Otherwise, the string will contain all full names with "/" in between.
#' @export
#'
#' @examples
#' combine_names(c("Amelanchier humilis", "Amelanchier sanguinea"))
combine_names <- function(x) {
  parts <- stringr::str_split_fixed(x, " ", 2)   # split into genus + species part
  genera <- unique(parts[,1])
  if(length(genera) == 1) { # Only one genus → keep genus for first element, then species epithets only
    paste0(genera, " ", paste0(parts[,2][1], "/", paste(parts[-1,2],
                                                        collapse = "/")))
  } else { # Multiple genera → keep full names
    paste(x, collapse = "/")
  }
}
