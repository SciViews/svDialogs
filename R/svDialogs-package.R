#' SciViews - Standard Dialog Boxes for R
#'
#' Rapidly construct standard dialog boxes for your GUI, including
#' message boxes, input boxes, list, file or dir selection, ... In case R cannot
#' display GUI dialog boxes, a simpler command line version of these interactive
#' elements is also provided as fallback solution.
#'
#' @section Important functions:
#'
#'- [dlg_message()] display a message box,
#'- [dlg_input()] prompt for textual input,
#'- [dlg_list()] select one or more items in a list,
#'- [dlg_open()] open one or more existing file(s),
#'- [dlg_save()] prompt for a file to save to (and ask confirmation if the file
#'already exists),
#'- [dlg_dir()] select a directory,
#'- [dlg_form()] display a complex dialog box with textual areas, password,
#'checkboxes, comboboxes, font and size, color, etc.
#'
#' @note Under Linux, you should install **'yad'** (preferably), or
#' **'zenity'**. If you use 'zenity', you cannot build more complex [dlg_form()]
#' dialog boxes, and some versions trigger a warning like: "Gtk-Message:
#' GtkDialog mapped without a transient parent. This is discouraged." You do not
#' get these inconveniences if you use 'yad' (default choice in case both
#' programs are installed simultaneously).

#' @docType package
#' @name svDialogs-package
#'
#' @import utils
#' @importFrom methods as
#' @import rstudioapi
#' @import svGUI
NULL
