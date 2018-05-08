#' Determine the appropriate GUI to instantiate
#'
#' @param rstudio Logical. Should RStudio dialog automatically be used if
#'   available?
#' @return A character scalar giving either "RStudio" or the result of
#'   `Sys.info()["sysname"]`
#' @keywords internal
get_syst <- function(rstudio = TRUE) {
  is_rstudio <- .is_rstudio()
  is_desktop <- FALSE
  if (is_rstudio) {
    is_desktop <- rstudioapi::versionInfo()$mode == "desktop"
  }

  if (all(is_rstudio, !is_desktop)) {
    syst <- "RStudio"
    if (!rstudio) {
      message(paste("rstudio = FALSE only supported for RStudio Desktop.",
        "Treating as TRUE."))
    }
  } else {
    syst <- Sys.info()["sysname"]
  }

  if (all(syst != "RStudio", is_rstudio, rstudio)) syst <- "RStudio"
  return(syst)
}
