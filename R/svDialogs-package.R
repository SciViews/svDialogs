#' @details
#' The dialog box function dispatch to specific dialog boxes depending on the OS
#' and whether a GUI can be displayed or not.
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
#'
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
#' @import utils
#' @importFrom methods as
#' @import rstudioapi
#' @import svGUI
NULL
