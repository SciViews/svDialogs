#' Determine the appropriate GUI to instantiate
#'
#' @param rstudio Logical. Should RStudio dialog automatically be used if available?
#' @return A character scalar giving either "RStudio" or the result of
#'   `Sys.info()["sysname"]`
#' @keywords internal
get_syst <- function(rstudio = TRUE) {
  is_rstudio <- .is_rstudio()
  is_desktop <- FALSE
  if (is_rstudio) {
    is_desktop <- rstudioapi::versionInfo()$mode == "desktop"
  }

  if (is_rstudio & (!is_desktop)) {
    syst <- "RStudio"
  } else {
    syst <- Sys.info()["sysname"]
  }

  if (syst != "RStudio" & rstudio) syst <- "RStudio"
  return(syst)
}
