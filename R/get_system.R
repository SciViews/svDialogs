#' Determine the appropriate GUI to instantiate
#'
#' The native version of the dialog box displays different versions depending
#' on the OS (Linux, Windows or MacOS). In the case of 'RStudio', its own dialog
#' boxes are used, if they exist. However, in 'RStudio Desktop' only, and when
#' the argument `rstudio = FALSE` is set, OS dialog boxes may be forced, and
#' the function would return the OS instead of `RStudio`.
#'
#' @param rstudio Logical. Should 'RStudio' dialog boxes automatically be used
#'   if available? If `FALSE`, force using OS dialog boxes, but only in
#'   'RStudio Desktop' (ignored in 'RStudio Server'). Can be changed globally
#'   with `options(svDialogs.rstudio = TRUE|FALSE)`. `TRUE` by default.
#' @return A character scalar giving either "RStudio" or the result of
#'   `Sys.info()["sysname"]`
#' @keywords internal
get_system <- function(rstudio = getOption("svDialogs.rstudio", TRUE)) {
  syst <- Sys.info()["sysname"][[1]]
  if (.is_rstudio()) {
    # One can switch to OS dialog boxes only in RStudio desktop
    # So, in all other case, we return 'RStudio'
    if (isTRUE(rstudio) || !.is_rstudio_desktop())
      syst <- "RStudio"
  }

  syst
}
