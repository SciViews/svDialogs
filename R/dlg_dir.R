#' Modal dialog to select a directory.
#'
#' Select an existing directory, or create a new one.
#'
#' @param default The path to the default directory that is proposed (e.g.,
#' current working directory).
#' @param title A title to display on top of the dialog box.
#' @param ... Pass further arguments to methods.
#' @param gui The 'gui' object concerned by this dialog box.
#' @return The modified 'gui' object is returned invisibly. Use its `gui$res`
#' component to get the returned directory (the string is empty when the user
#' cancelled the dialog box, see example).
#' @export
#' @name dlg_dir
#' @seealso [dlg_open()], [dlg_save()]
#' @keywords misc
#' @concept Modal dialog box
#' @examples
#' \dontrun{
#' # A quick default directory changer
#' setwd(dlg_dir(default = getwd())$res)
#' }
dlg_dir <- function(default = getwd(), title, ..., gui = .GUI) {
  # Define the S3 method
  if (!gui$startUI("dlg_dir", call = match.call(), default = default,
    msg = "Displaying a modal dir selection dialog box",
    msg.no.ask = "A modal dir selection dialog box was by-passed"))
    return(invisible(gui))

  # Check and rework main arguments and place them in gui$args
  if (!length(default)) default <- getwd() else
    default <- as.character(default)[1]
  if (file.exists(default))
    if (!file.info(default)$isdir) default <- dirname(default) # Need a dir
    default <- path.expand(default)
    # Under Windows, it uses \\ as separator, although .Platform$file.sep
    # is now / (tested in R 2.11.1) => replace it
    if (.Platform$OS.type == "windows")
      default <- gsub("\\\\", "/", default)
    if (missing(title) || !length(title) || title == "") {
      title <- "Choose a directory"
    } else {
      title <- paste(as.character(title), collapse = "\n")
    }
    gui$setUI(args = list(default = default, title = title))

    # ... and dispatch to the method
    UseMethod("dlgDir", gui)
}

#' @export
#' @rdname dlg_dir
dlgDir <- dlg_dir # Backward compatibility

#' @export
#' @rdname dlg_dir
dlgDir.gui <- function(default = getwd(), title, ..., gui = .GUI) {
  # Used to break the chain of NextMethod(), searching for a usable method
  # in the current context
  msg <- paste("No workable method available to display",
    "a dir selection dialog box using:")
  msg <- paste(msg, paste(guiWidgets(gui), collapse = ", "))
  gui$setUI(status = "error", msg = msg, widgets = "none")
  stop(msg)
}

#' @export
#' @rdname dlg_dir
dlgDir.textCLI <- function(default = getwd(), title, ..., gui = .GUI) {
  # The pure textual version used a fallback in case no GUI could be used
  gui$setUI(widgets = "textCLI")
  # Ask for the directory
  res <- readline(paste0(gui$args$title,
    " [", gui$args$default, "] or 0 to cancel: "))
  if (res == "0") {
    res <- character(0) # User cancelled the action
  } else {
    if (res == "") res <- gui$args$default else res <- res
    # In case we pasted a string with single, or double quotes, or spaces
    # eliminate them
    res <- sub("^['\" ]+", "", sub("['\" ]+$", "", res))
    res <- path.expand(res)
    # To get the same behaviour as the GUI equivalents, we must make sure
    # it is a directory, or try to create it (possibly recursively, if it
    # does not exist). Also return absolute path
    if (file.exists(res)) {
      # Check that this is a directory, not a file!
      if (!file.info(res)$isdir) {
        warning(res, " is not a directory")
        res <- character(0) # Same as if the user did cancel the dialog box
      }
    } else {
      # The directory does not exists, try to create it now...
      dir.create(res, recursive = TRUE)
      if (!file.exists(res) || !file.info(res)$isdir) {
        warning("Error while creating the directory ", res)
        res <- character(0)
      }
    }
    if (length(res))
      res <- gsub("\\\\", "/", normalizePath(res))
  }
  gui$setUI(res = res, status = NULL)
  invisible(gui)
}

#' @inheritParams get_syst
#' @export
#' @rdname dlg_dir
dlgDir.nativeGUI <- function(default = getwd(), title, rstudio = TRUE, ..., gui = .GUI) {
  # The native version of the dir select box
  gui$setUI(widgets = "nativeGUI")
  # A 'choose a directory' dialog box
  # It almost follows the conventions of tkchooseDirectory()
  # The argument default indicates the initial directory
  # If cancelled, then return character(0)
  # This dialog box is always modal
  syst <- get_syst(rstudio)
  res <- switch(syst,
    RStudio = .rstudio_dlg_dir(gui$args$default, gui$args$title),
    Windows = .win_dlg_dir(gui$args$default, gui$args$title),
    Darwin  = .mac_dlg_dir(gui$args$default, gui$args$title),
    .unix_dlg_dir(gui$args$default, gui$args$title, ...)
  )

  # Do we need to further dispatch?
  if (is.null(res)) NextMethod("dlgDir", gui) else {
    gui$setUI(res = res, status = NULL)
    invisible(gui)
  }
}

# RStudio version
.rstudio_dlg_dir <- function(default = getwd(), title = "") {
  res <- rstudioapi::selectDirectory(caption = title, path = default)
  if (is.null(res)) {
    res <- character(0)
  } else{
    res <-  path.expand(gsub("\\\\", "/", res))
  }
  res
}

# Windows version
.win_dlg_dir <- function(default = getwd(), title = "") {
	res <- choose.dir(default = default, caption = title)
  if (is.na(res)) {
    res <- character(0)
  } else {
    res <-  path.expand(gsub("\\\\", "/", res))
  }
	res
}

# MacOS version
.mac_dlg_dir <- function(default = getwd(), title = "") {
  # Display a modal directory selector with native Mac dialog box
  #if (.Platform$GUI == "AQUA") app <- "(name of application \"R\")" else
  # This works from Mac OS X 10.5 Leopard:
  if (.Platform$GUI == "AQUA") {
    app <- "(name of application id \"Rgui\")"
  } else if (.is_jgr()) {
    app <- "\"JGR\""
  } else {
    app <- "\"Terminal\""
  }
  # Avoid displaying warning message when the user clicks on 'Cancel'
  owarn <- getOption("warn")
  on.exit(options(warn = owarn))
  options(warn = -1)
  if (title == "") mcmd <- "" else mcmd <- paste("with prompt \"",
    .replace_quotes(title), "\" ", sep = "")
  cmd <- paste0("-e 'tell application ", app,
    " to set foldername to choose folder ", mcmd, "default location \"",
    default , "\"' -e 'POSIX path of foldername'")
  # I cannot use system(intern = TRUE) with this in R.app/R64.app
  # (deadlock situation?), but I can in R run in a terminal. system2() also
  # works, but this preclue of using svDialogs on R < 2.12.0.
  # The hack is thus to redirect output to a file, then, to read the content
  # of that file and to desctroy it
  tfile <- tempfile()
  on.exit(unlink(tfile))
  res <- try(system(paste("osascript", cmd, ">", tfile), wait = TRUE,
    intern = FALSE, ignore.stderr = TRUE), silent = TRUE)
  if (inherits(res, "try-error") || !length(res))
    return(character(0))
  if (res > 0)
    return(character(0)) # User cancelled input
  path.expand(readLines(tfile))
}

# Linux/Unix version
.unix_dlg_dir <- function(default = getwd(), title = "", zenity = FALSE) {
  exec <- .get_yad_or_zenity(zenity)
  if (exec == "")
    return(NULL) # Try next method
  is_yad <- attr(exec, "is_yad")
  exec <- as.character(exec)
  # Avoid displaying warning message in case user clicks on Cancel
  owarn <- getOption("warn")
  on.exit(options(warn = owarn))
  options(warn = -1)
  # Use zenity to display the directory selection
  # There is no message area here, but one can set the title
  if (title == "") {
    title <- "Choose a directory" # Default title
  } else {
    title <- .escape_quotes(title)
  }
  msg <- paste0("'", exec, "' --file-selection --title=\"", title,
    "\" --directory --filename=\"", default, "\"")
  if (is_yad)
    msg <- paste(msg, "--on-top --skip-taskbar")
  res <- system(msg, intern = TRUE)
  if (length(res)) {
    res <- path.expand(res)
  } else res <- character(0)
  res
}
