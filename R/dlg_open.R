#' Modal dialog to select a file.
#'
#' Select an existing file, or create a new one.
#'
#' @param default The default file to start with (use `/dir/*` or `/dir/*.*` to
#' start in a given directory).
#' @param title A title to display on top of the dialog box.
#' @param multiple Is a multiple selection of files allowed?
#' @param filters A specification of file filters as a `nx2` matrix, or a
#' character string with even number of items. First items is the label, second
#' one is the filter. See `dlg_filters` for examples. This is currently ignored
#' on MacOS, since such kind of filter is defined differently there.
#' @param ... Pass further arguments to methods.
#' @param gui The 'gui' object concerned by this dialog box.
#' @return The modified 'gui' object is returned invisibly. The chosen file(s),
#' or an empty string if the "cancel" button was clicked is found in `gui$res`
#' (see example).
#' @export
#' @name dlg_open
#' @seealso [dlg_save()], [dlg_dir()]
#' @keywords misc
#' @concept Modal dialog box
#' @examples
#' \dontrun{
#' # Choose one R file
#' dlg_open(title = "Select one R file", filters = dlg_filters[c("R", "All"), ])$res
#' # Choose several files
#' dlg_open(multiple = TRUE)$res
#' }
dlg_open <- function(default, title, multiple = FALSE,
filters = dlg_filters["All", ], ..., gui = .GUI) {
  # Define the S3 method
  # An 'open file(s)' dialog box
  # title is used as caption of the dialog box
  # defaultFile allows to preselect a file
  # defaultDir opens the dialog box in that directory
  # multi indicates if multiple selection is allowed
  # filters is a n x 2 matrix of characters with description and filter
  # for instance: "R or S files (*.R, *.q)"       "*.R;*.q"
  # It could be also an even number of character strings that will be
  # reorganized into a n x 2 matrix.
  # Note that caption is also accepted and is then mapped to title
  # (for compatibility with choose.files()), but index is ignored: it is
  # always the first filter that is selected by default in the dialog box
  # To specify an initial dir, but no initial file, use /dir/*.*

  if (missing(default) || !length(default))
    default <- character(0)
  if (!gui$startUI("dlgOpen", call = match.call(), default = default,
    msg = "Displaying a modal open file dialog box",
    msg.no.ask = "A modal open file dialog box was by-passed"))
    return(invisible(gui))

  # Check and rework main arguments and place them in gui$args
  if (missing(default) || !length(default))
    default <- file.path(path.expand(getwd()), "*.*")
  default <- as.character(default)[1]
  # Under Windows, it uses \\ as separator, although .Platform$file.sep
  # is now / (tested in R 2.11.1) => replace it
  if (.Platform$OS.type == "windows")
    default <- gsub("\\\\", "/", default)
  # Check that dir and file already exists
  dir <- dirname(default)
  if (!file.exists(dir) || !file.info(dir)$isdir)
    default <- file.path(getwd(), basename(default))
  # Check that file exists
  file <- basename(default)
  if (file != "*.*" && file != "*" && !file.exists(default))
    default <- file.path(dirname(default), "*.*")
  multiple <- isTRUE(as.logical(multiple))
  if (missing(title) || !length(title) || title == "") {
    if (multiple) {
      title <- "Select files"
    } else {
      title <- "Select file"
    }
  } else title <- as.character(title)[1]
  # Check that filter is a nx2 character matrix, or try reshape it as such
  if (is.matrix(filters)) {
    if (ncol(filters) != 2 || !is.character(filters))
      filters <- dlg_filters["All", , drop = FALSE]
  } else if (length(filters) %% 2 != 0) {
    filters <- dlg_filters["All", , drop = FALSE]
  } else {# Try to reshape it
    filters <- matrix(as.character(filters), ncol = 2, byrow = TRUE)
  }
  gui$setUI(args = list(default = default, title = title,
    multiple = multiple, filters = filters))

  # ... and dispatch to the method
  UseMethod("dlgOpen", gui)
}

#' @export
#' @rdname dlg_open
dlgOpen <- dlg_open # Backward compatibility

#' @export
#' @rdname dlg_open
# Default filters for dlg_open() and dlg_save() boxes
dlg_filters <- matrix(c(
  "R or S files (*.R,*.q,*.ssc,*.S)", "*.R;*.q;*.ssc;*.S",
  "Enhanced metafiles (*.emf)","*.emf",
  "Postscript files (*.ps)", "*.ps",
  "PDF files (*.pdf)", "*.pdf",
  "Png files (*.png)", "*.png",
  "Windows bitmap files (*.bmp)", "*.bmp",
  "Jpeg files (*.jpeg,*.jpg)", "*.jpeg;*.jpg",
  "Text files (*.txt)", "*.txt",
  "R images (*.RData,*.rda)", "*.RData;*.rda",
  "Zip files (*.zip)", "*.zip",
  "All files (*.*)", "*.*" ), ncol = 2, byrow = TRUE)

rownames(dlg_filters) <- c("R", "emf", "ps", "pdf", "png", "bmp", "jpeg",
  "txt", "RData", "zip", "All")

#' @export
#' @rdname dlg_open
dlgFilters <- dlg_filters # Backward compatibility

#' @export
#' @rdname dlg_open
dlgOpen.gui <- function(default, title, multiple = FALSE,
filters = dlg_filters["All", ], ..., gui = .GUI) {
  # Used to break the chain of NextMethod(), searching for an usable method
  # in the current context
  msg <- paste("No workable method available to display",
    "a file open dialog box using:")
  msg <- paste(msg, paste(guiWidgets(gui), collapse = ", "))
  gui$setUI(status = "error", msg = msg, widgets = "none")
  stop(msg)
}

#' @export
#' @rdname dlg_open
dlgOpen.textCLI <- function(default, title, multiple = FALSE,
filters = dlg_filters["All", ], ..., gui = .GUI) {
  # The pure textual version used as fallback in case no GUI could be used
  # TODO: there is a problem with /dir/*.* => return => use it as a default
  # and then, issues a warning that the file does not exist!
  gui$setUI(widgets = "textCLI")
  # Ask for the file
  res <- readline(paste0(gui$args$title, " [", gui$args$default, "]: "))
  if (res == "") {
    res <- gui$args$default
  } else {
    res <- res
  }
  # Multiple files are separated by commas
  res <- strsplit(res, ",")[[1]]
  # In case we pasted a string with single, or double quotes, or spaces
  # eliminate them
  res <- sub("^['\" ]+", "", sub("['\" ]+$", "", res))
  res <- res[res != ""]
  # If we have serveral files returned, but multiple is FALSE, keep only
  # first one with a warning
  if (!gui$args$multiple && length(res) > 1) {
    warning("Only one file was expected... using only the first one")
    res <- res[1]
  }
  # Check that the file(s) exist
  is_there <- file.exists(res)
  if (!any(is_there)) {
    warning("File(s) do not exist")
    res <- character(0) # Same as if the user did cancel the dialog box
  } else {
    # Keep only existing files
    if (!all(is_there))
      warning("There are inexistent files that will be ignored")
    res <- res[is_there]
  }
  if (length(res))
    res <- normalizePath(res)
  gui$setUI(res = res, status = NULL)
  invisible(gui)
}

#' @export
#' @rdname dlg_open
dlgOpen.nativeGUI <- function(default, title, multiple = FALSE,
filters = dlg_filters["All", ], ..., gui = .GUI) {
  # The native version of the file open box
  gui$setUI(widgets = "nativeGUI")
  # An 'open file' dialog box
  # If cancelled, then return character(0)
  # This dialog box is always modal
  #
  # Replacement for choose.files(), tkgetOpenFile() & file.choose(new=FALSE)
  res <- switch(Sys.info()["sysname"],
    Windows = .win_dlg_open(gui$args$default, gui$args$title,
      gui$args$multiple, gui$args$filters),
    Darwin = .mac_dlg_open(gui$args$default, gui$args$title,
      gui$args$multiple, gui$args$filters),
    .unix_dlg_open(gui$args$default, gui$args$title,
      gui$args$multiple, gui$args$filters)
  )

  # Do we need to further dispatch?
  if (is.null(res)) {
    NextMethod("dlgOpen", gui)
  } else {
    gui$setUI(res = res, status = NULL)
    invisible(gui)
	}
}

# Windows version
.win_dlg_open <- function(default, title, multiple = FALSE,
filters = dlg_filters["All", ]) {
  if (!is.matrix(filters))
    filters <- matrix(filters, ncol = 2, byrow = TRUE)
  if (missing(default) || !length(default))
    default <- ""
  res <- choose.files(default = default, caption = title, multi = multiple,
    filters = filters, index = 1)
  if (length(res))
    res <-  gsub("\\\\", "/", res)
  if (length(res) == 1 && res == "")
    res <- character(0)
  res
}

# MacOS version
.mac_dlg_open <- function(default, title, multiple = FALSE,
filters = dlg_filters["All", ]) {
  # TODO: filters are implemented differently on the Mac => how to do this???
  if (!is.matrix(filters))
    filters <- matrix(filters, ncol = 2, byrow = TRUE)
  # Display a modal file open selector with native Mac dialog box
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
  if (title == "") {
    mcmd <- ""
  } else {
    mcmd <- paste0("with prompt \"", title, "\" ")
  }
  if (multiple)
    mcmd <- paste(mcmd, "multiple selections allowed true")
  if (!is.null(default) && default != "") {
    # Default must be an existing file or dir... otherwise, the cmd fails!
    if (!file.exists(default))
      default <- dirname(default)
    # try a second time...
    if (!file.exists(default))
      default <- dirname(default)
    if (file.exists(default))
      mcmd <- paste0(mcmd, " default location \"", default, "\"")
  }
  if (multiple) {
    cmd <- paste0("-e 'tell application ", app,
      " to set filenames to choose file ", mcmd,
      "' -e 'set res to \"\"' -e 'repeat with filename in filenames'",
      " -e 'set res to res & (POSIX path of filename) & \"\n\"'",
      " -e 'end repeat' -e 'res'")
  } else {
    cmd <- paste0("-e 'tell application ", app,
      " to set filename to choose file ", mcmd,
      "' -e 'POSIX path of filename'")
  }
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
  res <- readLines(tfile)
  res <- res[res != ""] # Eliminate empty lines
  res
}

# Linux/Unix version
.unix_dlg_open <- function(default, title, multiple = FALSE,
filters = dlg_filters["All", ]) {
  # Note: only existing filenames can be selected as default, otherwise, the
  # argument is ignored!
  # zenity must be installed on this machine!
  if (Sys.which("zenity") == "")
    return(NULL)
  # Avoid displaying warning message in case user clicks on Cancel
  owarn <- getOption("warn")
  on.exit(options(warn = owarn))
  options(warn = -1)
  # Use zenity to display the file open selection
  # Construct the -file-filter options
  if (multiple) {
    fcmd <- "--multiple"
  } else {
    fcmd <- ""
  }
  if (!is.matrix(filters))
    filters <- matrix(filters, ncol = 2, byrow = TRUE)
  nf <- nrow(filters)
  if (nf > 0)
    for (i in 1:nf)
      fcmd <- paste0(fcmd, " --file-filter=\"", filters[i, 1], " | ",
        gsub(";", " ", filters[i, 2]), "\"")
  msg <- paste0("zenity --file-selection --title=\"", title,
    "\" --filename=\"", default, "\" ", fcmd)
  res <- system(msg, intern = TRUE)
  if (!length(res)) {
    character(0)
  } else {
    strsplit(res, "|", fixed = TRUE)[[1]]
  }
}
