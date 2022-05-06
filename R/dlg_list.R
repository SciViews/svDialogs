#' Modal dialog to select one or more items in a list.
#'
#' Display a list and allow user to select either one, or multiple items in that
#' list.
#'
#' @param choices The list of items. It is coerced to character strings.
#' @param preselect A list of preselections, or `NULL` (then, the first element
#' is selected in the list). Preselections not in choices are tolerated (but
#' they are ignored without warning or error).
#' @param multiple Is it a multiple selection dialog box?
#' @param title The title of the dialog box, or `NULL` to use a default title
#' instead.
#' @param ... Pass further arguments to methods.
#' @param gui The 'gui' object concerned by this dialog box.
#' @return The modified 'gui' object is returned invisibly. A list with selected
#' items, or a character vector of length 0 if the dialog box was cancelled is
#' available from `gui$res` (see examples).
#' @note RStudio does not provide (yet) a graphical list selector (as of version
#' 1.1.447). Consequently, a Tk version is used (if 'tcltk' is available) for
#' 'RStudio Desktop' and a textual version at the R Console is used in the other
#' cases, for `'nativeGUI'` as a temporary workaround (should be implemented in
#' Shiny later on). Also note that the textual version only reports preselection
#' when `multiple == TRUE`, and they are not used automatically if you do not
#' respecify them in your feedback (limitation of
#' `utils::select.list(graphics = FALSE)`).
#' On MacOS, and outside of R(64).app, which has his own list selection dialog
#' box, single and double quotes are temporarily replaced by their slanted
#' versions (unicode characters u3032 and u2033, respectively) because the
#' command that triggers the dialog box does not allow quotes inside strings.
#' Regular quotes are reset on the output. This is the only hack we found that
#' was working. Better solutions are welcome, of course!
#' @export
#' @name dlg_list
#' @seealso [dlg_form()], [dlg_input()]
#' @keywords misc
#' @concept Modal dialog box
#' @examples
#' \dontrun{
#' # Select one or several months
#' res <- dlg_list(month.name, multiple = TRUE)$res
#' if (!length(res)) {
#'   cat("You cancelled the choice\n")
#' } else {
#'   cat("You selected:\n")
#'   print(res)
#' }
#' }
dlg_list <- function(choices, preselect = NULL, multiple = FALSE, title = NULL,
..., gui = .GUI) {
  # Define the S3 method
  choices <- as.character(choices)
  if (!length(choices))
    return(character(0)) # Nothing to select
  preselect <- as.character(preselect)
  preselect <- preselect[preselect %in% choices]
  if (!length(preselect))
    preselect <- choices[1] # Select first item by default

  # Start a GUI action... or by-pass it
  if (!gui$startUI("dlg_list", call = match.call(), default = preselect,
    msg = "Displaying a modal list dialog box",
    msg.no.ask = "A modal list dialog box was by-passed"))
    return(invisible(gui))

  # Further argument checking
  multiple <- isTRUE(as.logical(multiple))
  if (!length(title)) {
    title <- NULL
  } else {
    title <- as.character(title)[1]
  }
  gui$setUI(args = list(choices = choices, preselect = preselect,
    multiple = multiple, title = title))

  # ... and dispatch to the method
  UseMethod("dlg_list", gui)
}

#' @export
#' @rdname dlg_list
dlgList <- dlg_list # Backward compatibility

#' @export
#' @rdname dlg_list
dlg_list.gui <- function(choices, preselect = NULL, multiple = FALSE,
title = NULL, ..., gui = .GUI) {
  # Used to break the chain of NextMethod(), searching for a usable method
  # in the current context
  msg <- paste("No workable method available to display",
    "a list dialog box using:")
  msg <- paste(msg, paste(guiWidgets(gui), collapse = ", "))
  gui$setUI(status = "error", msg = msg, widgets = "none")
  stop(msg)
}

#' @export
#' @rdname dlg_list
dlg_list.textCLI <- function(choices, preselect = NULL, multiple = FALSE,
title = NULL, ..., gui = .GUI) {
  # The pure textual version used a fallback in case no GUI could be used
  gui$setUI(widgets = "textCLI")
  # Ask a selection in a textual menu
  choices <- gui$args$choices
  multiple <- gui$args$multiple
  if (multiple) {
    title2 <- "- Select one or more or 0 to cancel"
  } else {
    title2 <- "- Select one or 0 to cancel"
  }
  res <- select.list(choices = choices, preselect = gui$args$preselect,
    multiple = multiple, title = paste(gui$args$title, title2),
    graphics = FALSE)
  # When multiple is FALSE and user cancelled, returns "" instead of
  # character(0) => change this for consistency
  if (!multiple && res == "" && !"" %in% choices)
    res <- character(0)
  gui$setUI(res = res, status = NULL)
  invisible(gui)
}

#' @inheritParams get_system
#' @export
#' @rdname dlg_list
dlg_list.nativeGUI <- function(choices, preselect = NULL, multiple = FALSE,
title = NULL, rstudio = getOption("svDialogs.rstudio", TRUE), ..., gui = .GUI) {
  # The native version of the list box
  gui$setUI(widgets = "nativeGUI")
  # This is a simple 'select in the list' dialog box
  # It follows the syntax of the select.list() function
  res <- switch(get_system(rstudio),
    RStudio = .rstudio_dlg_list(gui$args$choices, gui$args$preselect,
      gui$args$multiple, gui$args$title),
    Windows = .win_dlg_list(gui$args$choices, gui$args$preselect,
      gui$args$multiple, gui$args$title),
    Darwin = .mac_dlg_list(gui$args$choices, gui$args$preselect,
      gui$args$multiple, gui$args$title),
    .unix_dlg_list(gui$args$choices, gui$args$preselect,
      gui$args$multiple, gui$args$title, ...)
  )

  # Do we need to further dispatch?
  if (is.null(res)) {
    NextMethod("dlg_list", gui)
  } else {
    gui$setUI(res = res, status = NULL)
    invisible(gui)
	}
}

# RStudio version (not yet, so, currently uses the non-graphical version)
.rstudio_dlg_list <- function(choices, preselect = NULL, multiple = FALSE,
title = NULL) {
  if (multiple) {
    title2 <- "- Select one or more"
  } else {
    title2 <- "- Select one"
  }
  graphics <- .is_rstudio_desktop()
  if (!graphics)
    title2 <- paste(title2, "or 0 to cancel")
  res <- select.list(choices = choices, preselect = preselect,
    multiple = multiple, title = paste(title, title2),
    graphics = graphics)
  if (length(res) == 1 && res == "") {
    character(0)
  } else {
    res
  }
}

# Windows version
.win_dlg_list <- function(choices, preselect = NULL, multiple = FALSE,
title = NULL) {
  # Windows version
  # select.list() does not have exactly the same behaviour, or native look&feel
  # on all R programs => redefine it here to make it more homogeneous
  # To get the same behaviour as under MacOS and Linux, that is, to return
  # character(0) when the user clicks on 'Cancel'
  res <- select.list(choices = choices, preselect = preselect,
    multiple = multiple, title = title, graphics = TRUE)
  if (length(res) == 1 && res == "") {
    character(0)
  } else {
    res
  }
}

# MacOS version
.mac_dlg_list <- function(choices, preselect = NULL, multiple = FALSE,
title = NULL) {
  if (.Platform$GUI == "AQUA") { # Use the R(64).app list box
    # Same as select.list(), but force graphics to TRUE
    # and always do preselection (first element) to match behaviour
    # under Windows
    if (is.null(preselect) || !any(preselect %in% choices))
      preselect <- choices[1]
    return(select.list(choices = choices, preselect = preselect,
      multiple = multiple, title = title, graphics = TRUE))
  } else if (.is_jgr()) {
    app <- "JGR"
  } else {
    app <- "Terminal"
  }
  # Use osascript to display the list box
  # The command does not allow quotes in string, so, temporailly replaced by
  # similar unicode characters...
  choices <- .replace_quotes(as.character(choices))
  if (!is.null(preselect))
    preselect <- .replace_quotes(as.character(preselect))
  # Make sure to keep only first preselection if !multiple
  if (!multiple)
    preselect <- preselect[1]
  # Format preselect into a single character string
  sel <- paste0('"', preselect, '  "', collapse = ",")
  # Format choices in a single string
  items <- paste0('"', choices, '  "', collapse = ",")
  # Default title
  if (is.null(title)) {
    if (multiple) {
      title <- "Select one or more"
    } else {
      title <- "Select one"
    }
  }
  # Avoid displaying warning message when the user clicks on 'Cancel'
  owarn <- getOption("warn")
  on.exit(options(warn = owarn))
  options(warn = -1)
  cmd <- paste0("-e 'tell application \"", app, "\" to choose from list {",
    items, "} with title \"Make your selection\" with prompt \"", title,
    "\" multiple selections allowed ", multiple, " default items {",
    sel, "}'")
  #res <- system2("osascript", cmd, stdout = TRUE, stderr = TRUE, wait = TRUE)
  res <- system(paste("osascript", cmd), intern = TRUE, wait = TRUE)
  if (res == "false") {
    character(0)
  } else {
    res <- unlist(strsplit(sub("  $", "", res), "  , ", fixed = TRUE))
    res <- .reset_quotes(res)
  }
}

# Linux/Unix version
.unix_dlg_list <- function(choices, preselect = NULL, multiple = FALSE,
title = NULL, zenity = FALSE) {
  choices <- .escape_quotes(as.character(choices))
  if (!is.null(preselect))
    preselect <- .escape_quotes(as.character(preselect))
  exec <- .get_yad_or_zenity(zenity)
  if (exec == "")
    return(NULL) # Try next method
  is_yad <- attr(exec, "is_yad")
  exec <- as.character(exec)
  # We don't use the ugly (on Linux) Tk version tk_select.list()
  # In zenity, the normal list mode do not allow for preselections
  # => switch to --checklist (multiple) or --radiolist (single) in this case
  # Make sure that we have at least one preselection
  if (multiple) {
    kind <- "--checklist --column=\"Pick\" --column=\"Item\" --multiple"
  } else {
    kind <- "--radiolist --column=\"Pick\" --column=\"Item\""
  }
  # Only one item is preselected if multiple is FALSE (keep first one)
  if (!multiple)
    preselect <- preselect[1]
  # Create a string with TRUE/FALSE item alternated
  sel <- choices %in% preselect
  items <- paste0(sel, ' "', choices, '"', collapse = " ")
  # Default title
  if (is.null(title)) {
    if (multiple) {
      title <- "Select one or more"
    } else {
      title <- "Select one"
    }
  }
  # Avoid warning when user clicks on 'Cancel'
  owarn <- getOption("warn")
  on.exit(options(warn = owarn))
  options(warn = -1)
  # Construct the command to send to zenity
  msg <- paste0("'", exec, "' --list --text=\"", title, "\" ", kind,
    " --hide-header --title=\"Make your choice\" --separator=\"@@@\" --height=",
    80 + 25 * length(choices), " ", items)
  if (is_yad)
    msg <- paste(msg, "--on-top --skip-taskbar --no-headers --print-column=2",
      "--search-column=2")
  res <- system(msg, intern = TRUE)
  res <- unlist(strsplit(res, "@@@", fixed = TRUE))
  if (is.null(res)) {
    character(0)
  } else {
    res
  }
}
