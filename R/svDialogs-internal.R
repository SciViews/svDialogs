.onLoad <- function(lib, pkg) {
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

.packageName <- "svDialogs"

.is_jgr <- function() "package:JGR" %in% search()

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
