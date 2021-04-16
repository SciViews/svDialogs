.onLoad <- function(lib, pkg) { # nocov start
  if (.tmpfiles_allowed()) {
    # Clear menus
    .menu_clear()
    # ... and create the default one
    .menu_file_init()
    .ctx_menu_file_init()
  }
  # If no Fix() function is found, assign a default one in the temp environment
  if (!exists("Fix") || !is.function(get("Fix")))
    .assign_temp("Fix", utils::fix)
}

.onUnload <- function(libpath) {
  # Clear menus
  if (interactive()) try(.menu_clear())
}

.GUI <- .GUI # The default .GUI object should be created by svGUI

.packageName <- "svDialogs"  # nocov end

.is_jgr <- function() "package:JGR" %in% search()

.is_rstudio <- function() rstudioapi::isAvailable()

.is_rstudio_desktop <- function() rstudioapi::versionInfo()$mode == "desktop"

# With yad or zenity, I cannot have double quotes inside strings: escape them
.escape_quotes <- function(str) {
  # For yad messages, we need to escape double quotes **inside** messages
  gsub('"', '\\"', str, fixed = TRUE)
}

# With MacOS, I cannot escape quotes: it does not work with dlg commands
# So, I temporarily replace them: U+2032 instead of ' and U+2033 for "
.replace_quotes <- function(str) {
  # Need to force toward UTF8
  str <- enc2utf8(str)
  str <- gsub("'", "\u2032", str, fixed = TRUE)
  gsub('"', "\u2033", str, fixed = TRUE)
}

# Do the opposite to .replace_quotes()
.reset_quotes <- function(str) {
  str <- gsub("\u2032", "'", str, fixed = TRUE)
  gsub("\u2033", '"', str, fixed = TRUE)
}

# Get the path to yad or zenity, or return "" otherwise
.get_yad_or_zenity <- function(zenity = FALSE) {
  if (!capabilities("X11"))
    return("")
  # Can use either yad (preferrably), or zenity
  exec <- as.character(Sys.which("yad"))
  is_yad <- TRUE
  if (exec == "" || zenity) {# yad not found, or force for zenity
    exec <- as.character(Sys.which("zenity"))
    is_yad <- FALSE
  }
  if (exec == "") {
    warning("The native directory selection dialog box is available",
      " only if you install 'yad' (preferrably), or 'zenity'")
  } else attr(exec, "is_yad") <- is_yad
  exec
}

# Avoid dependency on svMisc for those three functions
.temp_env <- function() {
  pos <-  match("SciViews:TempEnv", search())
  if (is.na(pos)) { # Must create it
    `SciViews:TempEnv` <- list()
    Attach <- function(...) get("attach", mode = "function")(...)
    Attach(`SciViews:TempEnv`, pos = length(search()) - 1)
    rm(`SciViews:TempEnv`)
    pos <- match("SciViews:TempEnv", search())
  }
  pos.to.env(pos)
}

.get_temp <- function(x, default = NULL, mode = "any", item = NULL) {
  if (is.null(item)) Mode <- mode else Mode <- "any"
  if  (exists(x, envir = .temp_env(), mode = Mode, inherits = FALSE)) {
    dat <- get(x, envir = .temp_env(), mode = Mode, inherits = FALSE)
    if (is.null(item)) return(dat) else {
      item <- as.character(item)[1]
      if (inherits(dat, "list") && item %in% names(dat)) {
        dat <- dat[[item]]
        if (mode != "any" && mode(dat) != mode) dat <- default
        return(dat)
      } else {
        return(default)
      }
    }
  } else {# Variable not found, return the default value
    return(default)
  }
}

.assign_temp <- function(x, value, replace.existing = TRUE)
  if (replace.existing || !exists(x, envir = .temp_env(), mode = "any",
    inherits = FALSE))
    assign(x, value, envir = .temp_env())
