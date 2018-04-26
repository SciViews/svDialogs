#' Modal dialog to select a file to save to.
#'
#' Select an existing file, or create a new one to save data.
#'
#' @param default The default file to start with (use `/dir/*` or `/dir/*.*` to
#' start in a given directory, but without predefined name).
#' @param title A title to display on top of the dialog box.
#' @param filters A specification of file filters as a `nx2` matrix, or a
#' character string with even number of items. First items is the label, second
#' one is the filter. See `dlg_filters` for examples. This is currently ignored
#' on MacOS, since such kind of filter is defined differently there.
#' @param ... Pass further arguments to methods.
#' @param gui The modified 'gui' object is returned invisibly. The chosen file,
#' or an empty string (if the "cancel" button was clicked or confirmation was
#' cancelled) is placed in `gui$res` (see example). For existing files,
#' confirmation is always asked!
#' @export
#' @note In case the file already exists, the user is prompted to confirm he
#' wants to overwrite the existing file. If he clicks `'Cancel'`, then the
#' return is an empty string. The 'RStudio' version of this dialog box currently
#' ignores the `filters =` argument.
#' @name dlg_save
#' @seealso [dlg_open()], [dlg_dir()]
#' @keywords misc
#' @concept Modal dialog box
#' @examples
#' \dontrun{
#' # Choose one R filename to save some R script into it
#' dlg_save(title = "Save R script to", filters = dlg_filters[c("R", "All"), ])$res
#' }
dlg_save <- function(default, title, filters = dlg_filters["All", ], ...,
gui = .GUI) {
  # Define the S3 method
  # TODO: define default extension!!!
  # A 'save file' dialog box
  # title is used as caption of the dialog box
  # default allows to preselect a file
  # Always ask for confirmation in case the file already exists
  # filters is a n x 2 matrix of characters with description and filter
  # for instance: "R or S files (*.R, *.q)"       "*.R;*.q"
  # It could be also an even number of character strings that will be
  # reorganized into a n x 2 matrix.
  if (missing(default) || !length(default))
    default <- "untitled"
  if (!gui$startUI("dlg_save", call = match.call(), default = default,
    msg = "Displaying a modal save file dialog box",
    msg.no.ask = "A modal save file dialog box was by-passed"))
    return(invisible(gui))

  # Check and rework main arguments and place them in gui$args
  if (missing(default) || is.null(default))
    default <- file.path(path.expand(getwd()), "untitled", sep = "")
  if (!length(default))
    default <- NULL
  if (!is.null(default)) {
    default <- as.character(default)[1]
    # Under Windows, it uses \\ as separator, although .Platform$file.sep
    # is now / (tested in R 2.11.1) => replace it
    if (.Platform$OS.type == "windows")
      default <- gsub("\\\\", "/", default)
    # Check that dir of default already exists
    dir <- dirname(default)
    # If not there, or not a dire, replace by current working dir...
    if (!file.exists(dir) || !file.info(dir)$isdir)
      default <- file.path(getwd(), basename(default))
  }
  if (missing(title) || title == "") {
    title <- "Save file as"
  } else {
    title <- as.character(title)[1]
  }
  # Check that filter is a nx2 character matrix, or try reshape it as such
  if (is.matrix(filters)) {
    if (ncol(filters) != 2 || !is.character(filters))
      filters <- NULL
  } else {
    if (length(filters) %% 2 != 0) {
      filters <- NULL
    } else {# Try to reshape it
      filters <- matrix(as.character(filters), ncol = 2, byrow = TRUE)
    }
  }
  gui$setUI(args = list(default = default, title = title, filters = filters))

  # ... and dispatch to the method
  UseMethod("dlgSave", gui)
}

#' @export
#' @rdname dlg_save
dlgSave <- dlg_save # Backward compatibility

#' @export
#' @rdname dlg_save
dlgSave.gui <- function(default, title, filters = dlg_filters["All", ], ...,
gui = .GUI) {
  # Used to break the chain of NextMethod(), searching for a usable method
  # in the current context
  msg <- paste("No workable method available to display",
    "a file save dialog box using:")
  msg <- paste(msg, paste(guiWidgets(gui), collapse = ", "))
  gui$setUI(status = "error", msg = msg, widgets = "none")
  stop(msg)
}

#' @export
#' @rdname dlg_save
dlgSave.textCLI <- function(default, title, filters = dlg_filters["All", ], ...,
gui = .GUI) {
  # The pure textual version used as fallback in case no GUI could be used
  gui$setUI(widgets = "textCLI")
  # Ask for the file
  res <- readline(paste0(gui$args$title,
    " [", gui$args$default, "] or 0 to cancel: "))
  if (res == "0") {
    res <- character(0) # User cancelled the action
  } else {
    if (res == "")
      res <- gui$args$default
    # In case we pasted a string with single, or double quotes, or spaces
    # eliminate them
    res <- sub("^['\" ]+", "", sub("['\" ]+$", "", res))
    if (length(res)) {
      res <- suppressWarnings(normalizePath(res))# Warning when no path is given
      # If file already exists => ask for confirmation...
      if (file.exists(res)) {
        choices <- c("ok", "cancel")
        ret <- select.list(choices,
          title = "Confirm you want to replace this file", graphics = FALSE)
        if (ret == "" || ret == "cancel")
          res <- character(0) # Cancelled
      }
    }
  }
  gui$setUI(res = res, status = NULL)
  invisible(gui)
}

#' @export
#' @rdname dlg_save
dlgSave.nativeGUI <- function(default, title, filters = dlg_filters["All", ],
..., gui = .GUI) {
  # The native version of the file save dialog box
  gui$setUI(widgets = "nativeGUI")
  # A 'save file' dialog box
  # If cancelled, then return character(0)
  # This dialog box is always modal
  #
  # It is a replacement for choose.files(), tkgetSaveFile()
  # & file.choose(new = TRUE), not implemented yet in R 2.14, by the way
  if (.is_rstudio()) syst <- "RStudio" else syst <- Sys.info()["sysname"]
  res <- switch(syst,
    RStudio = .rstudio_dlg_save(gui$args$default, gui$args$title,
      gui$args$filters),
    Windows = .win_dlg_save(gui$args$default, gui$args$title, gui$args$filters),
    Darwin = .mac_dlg_save(gui$args$default, gui$args$title, gui$args$filters),
    .unix_dlg_save(gui$args$default, gui$args$title, gui$args$filters, ...)
  )

  # Do we need to further dispatch?
  if (is.null(res)) {
    NextMethod("dlgSave", gui)
  } else {
    gui$setUI(res = res, status = NULL)
    invisible(gui)
  }
}

# RStudio version (need at least version 1.1.287)
.rstudio_dlg_save <- function(default = file.path(getwd(), "untitled"),
  title = "", filters = dlg_filters["All", ]) {
  if (rstudioapi::getVersion() < '1.1.287')
    return(NULL)
  # I don't understand how filter is used in selectFile(). So, I prefer **not**
  # to use it for now!
  res <- rstudioapi::selectFile(caption = title, path = default,
      label = "Save", existing = FALSE)
  if (is.null(res)) {
    res <- character(0)
  } else{
    res <-  path.expand(gsub("\\\\", "/", res))
  }
  res
}

# Windows version
.win_dlg_save <- function(default, title, filters = dlg_filters["All", ]) {
  # Note: this dialog box is a very bad design for saving as dialog box:
  # It displays the "Open" button and complains if the file does not exist!
  # TODO: should be replaced by the Tk version for now
  if (!is.matrix(filters))
    filters <- matrix(filters, ncol = 2, byrow = TRUE)
  if (!length(default))
    default <- ""
  res <- choose.files(default = default, caption = title,
    multi = FALSE, filters = filters, index = 1)
  if (length(res)) {
    res <-  gsub("\\\\", "/", res)
    if (file.exists(res) && tolower(winDialog(type = "okcancel",
      message = "The file already exists. It will be replaced!")) == "cancel")
      res <- character(0) # User cancelled!
  }
	res
}

# MacOS version
.mac_dlg_save <- function(default, title, filters = dlg_filters["All", ]) {
  title <- .replace_quotes(title)
  # TODO: filters are implemented differently on the Mac => how to do this???
  if (!is.matrix(filters))
    filters <- matrix(filters, ncol = 2, byrow = TRUE)
  # Display a modal file save selector with native Mac dialog box
  #if (.Platform$GUI == "AQUA") {
  #  app <- "(name of application \"R\")"
  #} else .....
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
  if (title == "") {
    mcmd <- ""
  } else {
    mcmd <- paste0("with prompt \"", title, "\" ")
  }
  if (length(default) && default != "") {
    # Default dir must be an existing dir... otherwise, the cmd fails!
    defdir <- dirname(default)
    if (!file.exists(defdir) || !file.info(defdir)$isdir)
      defdir <- getwd()
    mcmd <- paste0(mcmd, " default location \"", defdir, "\"")
    deffile <- basename(default)
    if (deffile != "*.*" && deffile != "*")
      mcmd <- paste0(mcmd, " default name \"", deffile, "\"")
  }
  cmd <- paste0("-e 'tell application ", app,
    " to set filename to choose file name ", mcmd,
    "' -e 'POSIX path of filename'")
  # I cannot use system(intern = TRUE) with this in R.app/R64.app
  # (deadlock situation?), but I can in R run in a terminal. system2() also
  # works, but this preclue of using svDialogs on R < 2.12.0.
  # The hack is thus to redirect output to a file, then, to read the content
  # of that file and to destroy it
  tfile <- tempfile()
  on.exit(unlink(tfile))
  res <- try(system(paste("osascript", cmd, ">", tfile), wait = TRUE,
    intern = FALSE, ignore.stderr = TRUE), silent = TRUE)
  if (inherits(res, "try-error") || !length(res))
    return(character(0))
  if (res > 0)
    return(character(0)) # User cancelled input
  res <- readLines(tfile)
  res <- res[res != ""] # Eliminate empty lines
  # Note: confirmation of replacement is built-in here
  res
}

# Linux/Unix version
# TODO: if no extension provided, displays '.' => make sure to change this!
.unix_dlg_save <- function(default, title, filters = dlg_filters["All", ],
zenity = FALSE) {
  # Note: only existing filenames can be selected as default, otherwise, the
  # argument is ignored!
  title <- .escape_quotes(title)
  exec <- .get_yad_or_zenity(zenity)
  if (exec == "")
    return(NULL) # Try next method
  is_yad <- attr(exec, "is_yad")
  exec <- as.character(exec)
  # Avoid displaying warning message in case user clicks on Cancel
  owarn <- getOption("warn")
  on.exit(options(warn = owarn))
  options(warn = -1)
  # Use zenity to display the file save selection
  # Construct the -file-filter options
  fcmd <- ""
  if (!is.matrix(filters))
    filters <- matrix(filters, ncol = 2, byrow = TRUE)
  nf <- nrow(filters)
  if (nf > 0)
    for (i in 1:nf)
      fcmd <- paste0(fcmd, " --file-filter=\"", filters[i, 1], " | ",
        gsub(";", " ", filters[i, 2]), "\"")
  msg <- paste0("'", exec, "' --file-selection --save --title=\"", title,
    "\" --filename=\"", default, "\" ", fcmd)
  if (is_yad)
    msg <- paste(msg, "--on-top --skip-taskbar")
  # ignore.stderr = TRUE because error if file not found!
  res <- system(msg, intern = TRUE, ignore.stderr = TRUE)
  if (!length(res)) {
    return(character(0))
  } else if (file.exists(res)) {# Ask for confirmation!
    if (is_yad) {
      msg <- paste0("'", exec, "' --image=gtk-dialog-question --text=\"",
        "This file already exists. It will be replaced!",
        "\" --button=Cancel:1 --button=OK:0 --title=\"Question\"",
        " --on-top --skip-taskbar")
    } else {
      msg <- paste0("'", exec, "' --question --text=\"",
        "This file already exists. It will be replaced!",
        "\" --ok-label=\"OK\" --cancel-label=\"Cancel\"",
        " --title=\"Question\"")
    }
    if (system(msg) > 0)
      return(character(0)) # Cancelled, or another error
  }
  strsplit(res, "|", fixed = TRUE)[[1]]
}
