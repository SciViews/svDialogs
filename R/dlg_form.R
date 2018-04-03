# You have to run yad to get this dialog box!
# The form is defined as a list
# TXT, H, RO, NUM, CHK, CB, CBE, FL, SFL, DIR, CDIR, FN, MFL, DT, CLR, BTN or LBL
# TXT = simple text entry (default type)
# H = hidden text (password)
# RO = read-only text
# NUM = null of positive integers with up/down arrows
# CHK = checkbox, return TRUE or FALSE!
# CB = read-only combobox
# CBE = editable combobox
# FL = open file item
# SFL = save file item
# DIR = select directory
# CDIR = select or create directory
# FN = select font and size
# MFL = select mutliple files
# DT = date (use --date-format=%F for YYYY-MM-DD)
# CLR = select a color (but no palette!)
# BTN = display a button... content is shell command to execute!
# LBL = display a label
# ex:
#form <- list(
#  "Name:TXT" = "some text",
#  "Age:NUM" = 25,
#  "Sex:CB" = c("male", "female"),
#  "Married:CHK"=TRUE
#)
# TODO: implement --image= argument of yad
# TODO: implement this dialog box on Windows and MacOS


#' Modal dialog to fill a series of different fields.
#'
#' A customizable form dialog box with checkboxes, entries, lists, etc.
#'
#' @param form Named list of default values, or list of possible items. Names
#' are the labels of corresponding entries in the dialog box, followed by an
#' indicator of the type of entry to place in the dialog box (see details).
#' @param title The title of the form.
#' @param message An optional message to display in the dialog box. Use
#' `\\n` for line break, or provide a vector of character strings, one for
#' each line.
#' @param columns Arrange the entries on this number of columns (by row).
#' @param strip.type Do we strip the type from the names in results?
#' @param ... Pass further arguments to methods.
#' @param gui The 'gui' object concerned by this dialog box.
#' @return The modified 'gui' object is returned invisibly. Use its `gui$res`
#' component to get the list of returned fields.
#' @details The form content is defined by a named list. Items are default
#' values, or a list of possible items, e.g., for the combobox. Names are labels
#' displayed in front of each field in the form. Follow them by a code that
#' represents the type of entry you want to use:
#'
#' - `:TXT` for simple (default) textual box,
#' - `:H` for hidden text (password),
#' - `:RO` for read-only text,
#' - `:NUM` for null of positive integers with up/down arrows,
#' - `:CHK` for checkbox: `TRUE` or `FALSE`,
#' - `:CB` for read-only combobox,
#' - `:CBE` for editable combobox,
#' - `:FL` to select one existing file,
#' - `:MFL` to select multiple existing files,
#' - `:SFL` to select or create one file,
#' - `:DIR` to select a directory,
#' - `:CDIR` to select or create a directory,
#' - `:FN` to select font and font size,
#' - `:DT` to enter a date,
#' - :CLR` to enter a RGB color,
#' - :BTN` to create a button that execute some code,
#' - `:LBL` to add a label.
#'
#' For the moment, the form dialog box is only supported on Linux. You have to
#' install **yad** to get access to it. On Ubuntu, you do so by
#' `sudo apt-get install yad`. For other system, look at the documentation.
#' @export
#' @name dlg_form
#' @seealso [dlg_input()], [dlg_list()]
#' @keywords misc
#' @concept Modal dialog box
#' @examples
#' \dontrun{
#' # Ask a series of items at once in a dialog box
#' form <- list(
#'   "Name:TXT" = "John Smith",
#'   "Age:NUM" = 25,
#'   "Sex:CB" = c("male", "female"),
#'   "Married:CHK"=FALSE
#' )
#' dlg_form(form, "My data")$res
#' }
dlg_form <- function(form, title = "Fill the form", message = NULL,
columns = 1, strip.type = TRUE, ..., gui = .GUI) {
  # Define the S3 method
  if (!gui$startUI("dlgForm", call = match.call(),
    default = lapply(form, "[", 1),
    msg = "Displaying a modal form dialog box",
    msg.no.ask = "A modal form dialog box was by-passed"))
    return(invisible(gui))

  # Check and rework main arguments and place them in gui$args
  if (!is.list(form) || is.null(names(form)))
    stop("'form' must be a named list")
  if (!length(title)) {
    title <- "Fill the form"
  } else {
    title <- as.character(title)[1]
  }
  if (length(message))
    message <- paste0(message, collapse = "\n")
  columns <- as.integer(columns)
  if (!length(columns)) {
    columns <- 1
  } else {
    columns <- columns[1]
  }
  if (columns < 1)
    columns <- 1
  strip.type <- isTRUE(as.logical(strip.type))
  gui$setUI(args = list(form = form, title = title, message = message,
    columns = columns, strip.type = strip.type))

  # ... and dispatch to the method
  UseMethod("dlgForm", gui)
}

#' @export
#' @rdname dlg_form
dlgForm <- dlg_form # Backward compatibility

#' @export
#' @rdname dlg_form
dlgForm.gui <- function(form, title = "Fill the form", message = NULL,
columns = 1, strip.type = TRUE, ..., gui = .GUI) {
  # Used to break the chain of NextMethod(), searching for a usable method
  # in the current context
  msg <- paste("No workable method available to display",
    "a form dialog box using:")
  msg <- paste(msg, paste(guiWidgets(gui), collapse = ", "))
  gui$setUI(status = "error", msg = msg, widgets = "none")
  stop(msg)
}

#' @export
#' @rdname dlg_form
dlgForm.textCLI <- function(form, title = "Fill the form", message = NULL,
columns = 1, strip.type = TRUE, ..., gui = .GUI) {
  # The pure textual version used a fallback in case no GUI could be used
  gui$setUI(widgets = "textCLI")
  # Ask for the input for each form field using readline()
  cat("==", gui$args$title, "==\n")
  if (length(gui$args$message))
    cat(gui$args$message, "\n", sep = "")
  form <- gui$args$form
  if (is.null(form))
    return(list())
  res <- form
  # Special treatment for :CHK type
  is_check <- grepl(":CHK$", names(form))
  names(form) <- sub(":[A-Z]+$", "", names(form))
  # Do we strip type?
  if (strip.type)
    res <- form
  for (i in seq_along(form)) {
  if (is_check[i]) {
    if (isTRUE(as.logical(form[[i]])[1])) {
      def <- "[Y]/n"
    } else {
      def <- "[N]/y"
    }
    ans <- readline(paste0(names(form)[i], " ", def, ": "))
    if (ans == "")
      ans <- def
      # Put TRUE if answer starts with y or Y, FALSE otherwise
      res[[i]] <- (grepl("^[yY]", ans))
    } else {
      def <- as.character(form[[i]])
      def[1] <- paste0("[", def[1], "]")
      res[[i]] <- readline(paste0(names(form)[i],
        " ", paste(def, collapse = "/"), ": "))
      # TODO: how to enter multi-items here?
      # TODO: eliminate surrounding single or double quotes
      if (res[[i]] == "") {
        res[[i]] <- form[[i]][1]
      } else {
        res[[i]] <- as(res[[i]], class(form[[i]])[1])
      }
    }
  }
  # OK, Cancel or Redo?
  # TODO: the redo feature...
  ans <- readline("==\n[OK]/Cancel ")
  if (ans != "" && !grepl("^[oO]", ans))
    res <- list()
  gui$setUI(res = res, status = NULL)
  invisible(gui)
}

#' @export
#' @rdname dlg_form
dlgForm.nativeGUI <- function(form, title = "Fill the form", message = NULL,
columns = 1, strip.type = TRUE, ..., gui = .GUI) {
  # The native version of the input box
  gui$setUI(widgets = "nativeGUI")
  # A simple text input box using native window
  # Return either a string, or character(0) if 'Cancel' clicked
  res <- switch(Sys.info()["sysname"],
    Windows = .win_dlg_form(gui$args$form, gui$args$title, gui$args$message,
      gui$args$columns, gui$args$strip.type),
    Darwin = .mac_dlg_form(gui$args$form, gui$args$title, gui$args$message,
      gui$args$columns, gui$args$strip.type),
    .unix_dlg_form(gui$args$form, gui$args$title, gui$args$message,
      gui$args$columns, gui$args$strip.type)
  )

  # Do we need to further dispatch?
  if (is.null(res)) {
    NextMethod("dlgForm", gui)
  } else {
    gui$setUI(res = res, status = NULL)
    invisible(gui)
	}
}

# Windows version
.win_dlg_form <- function(form, title, message, columns, strip.type) {
  # Not yet => return NULL
  return(NULL)
}

# MacOS version
.mac_dlg_form <- function(form, title, message, columns, strip.type) {
  # Not yet => return NULL
  return(NULL)
}

 # Linux/Unix version
.unix_dlg_form <- function(form, title, message, columns, strip.type) {
  # yad must be installed on this machine!
  if (Sys.which("yad") == "")
    return(NULL)
  # Avoid displaying warning message in case user clicks on Cancel
  owarn <- getOption("warn")
  on.exit(options(warn = owarn))
  options(warn = -1)
  # Use yad to display the form dialog box
  if (!length(message)) {
    msg <- paste0("yad --form --title=\"", title,
      "\" --on-top --center --skip-taskbar --separator=\"@@@\"",
      " --item-separator=\"&&&\" --date-format=%F --columns=", columns)
  } else {# Message is provided
    msg <- paste0("yad --form --title=\"", title, "\" --text=\"", message,
      "\" --on-top --center --skip-taskbar --separator=\"@@@\"",
      " --item-separator=\"&&&\" --date-format=%F --columns=", columns)
  }
  # Add the definition of the various fields
  fields <- paste0("--field=", shQuote(names(form)), collapse = " ")
  # Add the default values
  default <- paste(shQuote(sapply(form, paste, collapse = "&&&")),
    collapse = " ")
  # Display the dialog box
  res <- system(paste(msg, fields, default), intern = TRUE)
  # Did the user cancelled the dialog box
  if (!length(res))
    return(list())
  # Reformat the result
  res <- strsplit(res, "@@@", fixed = TRUE)[[1]]
  # Replace results in initial form
  for (i in seq_along(form))
    form[[i]] <- as(strsplit(res[i], "&&&", fixed = TRUE)[[1]],
      class(form[[i]])[1])
  # Do we strip type?
  if (strip.type)
    names(form) <- sub(":[A-Z]+$", "", names(form))
  form
}
