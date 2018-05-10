#' Determine the appropriate GUI to instantiate
#'
#' @param rstudio Logical. Should RStudio dialog automatically be used if
#'   available?
#' @return A character scalar giving either "RStudio" or the result of
#'   `Sys.info()["sysname"]`
#' @keywords internal
get_system <- function(rstudio = TRUE) {
  syst <- Sys.info()["sysname"][[1]]
  if (.is_rstudio()) {
    # One can switch to OS dialog boxes only in RStudio desktop
    # So, in all other case, we return 'RStudio'
    if (rstudio || !.is_rstudio_desktop())
      syst <- "RStudio"
  }

  syst
}
