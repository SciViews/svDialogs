#' Manage custom R menus.
#'
#' Create, populate and rework custom R menus.
#'
#' @param menuname A character string naming a menu.
#' @param itemname A character string naming a menu item on an existing menu.
#' @param action A character string with the action to perform when the menu item
#' is selected, or `"none"` for no action. Use `"enable"` or `"disable"` to
#' activate or deactivate an existing menu item.
#' @return These function return `NULL` invisibly. They are used for their
#' side-effect of creating, changing, or deleting custom R menus.
#' @details On Windows, the function manages custom menus in RGui the same way
#' as `winMenuAdd()` and similar function do. Menus are added to the right and
#' new menu entries are added to the bottom of the menu. It is currently not
#' possible to add menus for **Rterm.exe** under Windows.
#'
#' On Unix/Linux, under Gnome, you must install a little Gtk2 program called
#' `ctxmenu`, as well as a few other utilities to manage the menu actions. You
#' can download corresponding files (GPL-2 license) and get further instructions
#' at the bottom of http://www.sciviews.org/SciViews-R/. The \R code in
#' **svDialogs** only creates menu configuration files in `~/.ctxmenu/tmp/` and
#' only in interactive \R session and after the user agrees to do so (unless
#' `options(svDialogs.tmpfiles = TRUE)`).
#' Once you installed these files, you can access the menus by setting up
#' keyboard shortcuts to activate main and context menus. The respective
#' commands are `ctxmenu-main` and `ctxmenu-context` and you can use the
#' preference panel to assign, e.g., `<shift-menu>` and `<ctrl-menu>`, or other
#' keyboard shortcuts to these commands. Once everything is set up, you should
#' see your menus appearing when a console where \R + **svDialogs** runs is the
#' active window and you hit these shortcuts (after you have defined at least
#' one custom menu). Note also that you can define custom context menus for
#' other applications too, see the `README` file in the `ctxmenu` download.
#'
#' On MacOS, these functions are not implemented yet (but see source of the
#' package for experimental code commented out and try the JGR version for a
#' first implementation there).
#On Mac OS X, AppleScript custom application folder is used by default. It
#can be used only with R.app and you can access it through Mac script menu
#displayed in menu bar (to activate it, open Utilities -> AppleScript editor,
#then, go to Preferences... and check 'Show script menu in menu bar'). Custom
#R menus will be visible as folders in this menu bar item only when R.app or
#R64.app is the formost application. there is an alternate interface using the
#XMenu menu bar applet. It works with both R.app and R run in a terminal, but
#you have to install and customize it first. Install XMenu from
#http://xmenu.en.softonic.com/mac. You should use the custom commands only for
#R custom menus, because svDialogs will erase everything in it everytime the
#package starts!
#Configure XMenu to display only User-Defined items, and name it \"R\". Select
#\"Folders before files\". For icons, best rendering is obtained with \"None,
#Big font\". For menu titles, select \"Text\" for entries that look like real
#menus. Be sure to check also "Start at login". Selection of XMenu instead of
#AppleScript menus is not automatic, but it can be enabled in two different
#ways: (1) by entering \code{option(useXMenu = TRUE)}, or by placing a \"R\"
#file or folder in '~/Library/Application Support/XMenu'.
#On Mac OS X, menus and menu items are sorted alphabetically. So, to respect
#a given order, choose the name of your menus and menu items carefully.
#Possibly prepend items with space(s) to push them up in the list.
#'
#' Action is treated as \R input (echoed at the command line, parsed and
#' executed), except if it is `"none"`. In this case, no action is run when the
#' menu item is selected (merely as a placeholder for future menu actions). You
#' can change the action of an existing menu by reissuing the command with a
#' different action argument.
#'
#' If the `menuname=` parameter of `menu_add_item()` does not exists, it is
#' automatically created. For creating submenus, separate successive menu names
#' with slashes. Use `"-"` as name for separation menus under Windows or
#' Unix/Linux.
#' @export
#' @name menu
#' @seealso [dlg_open()], [dlg_save()]
#' @keywords misc
#' @concept Modal dialog box
#' @examples
#' \dontrun{
#' # A quick default directory changer
#' setwd(dlg_dir(default = getwd())$res)
#' }
menu_names <- function() {
  if (.is_jgr()) return(.jgr_menu_names())
  res <- switch(Sys.info()["sysname"],
    Windows = winMenuNames(),
    Darwin = .mac_menu_names(),
    .unix_menu_names()
  )
  res
}

#' @export
#' @rdname menu
menuNames <- menu_names # Backward compatibility

#' @export
#' @rdname menu
menu_items <- function(menuname) {
  menuname <- .check_menu_name(menuname)

  if (.is_jgr()) return(.jgr_menu_items(menuname))
  res <- switch(Sys.info()["sysname"],
    Windows = winMenuItems(menuname),
    Darwin = .mac_menu_items(menuname),
    .unix_menu_items(menuname)
  )
  res
}

#' @export
#' @rdname menu
menuItems <- menu_items # Backward compatibility

#' @export
#' @rdname menu
menu_add <- function(menuname) {
  menuname <- .check_menu_name(menuname)

  if (.is_jgr()) return(invisible(.jgr_menu_add(menuname)))
  res <- switch(Sys.info()["sysname"],
    Windows = winMenuAdd(menuname),
    Darwin = .mac_menu_add(menuname),
    .unix_menu_add(menuname)
  )
  invisible(res)
}

#' @export
#' @rdname menu
menuAdd <- menu_add # Backward compatibility

#' @export
#' @rdname menu
menu_add_item <- function(menuname, itemname, action) {
  menuname <- .check_menu_name(menuname)

  if (.is_jgr())
    return(invisible(.jgr_menu_add_item(menuname, itemname, action)))
  res <- switch(Sys.info()["sysname"],
    Windows = .winMenuAddItem(menuname, itemname, action),
    Darwin = .mac_menu_add_item(menuname, itemname, action),
    .unix_menu_add_item(menuname, itemname, action)
  )
  invisible(res)
}

#' @export
#' @rdname menu
menuAddItem <- menu_add_item # Backward compatibility

#' @export
#' @rdname menu
menu_del <- function(menuname) {
  menuname <- .check_menu_name(menuname)

  if (.is_jgr()) return(invisible(.jgr_menu_del(menuname)))
  res <- switch(Sys.info()["sysname"],
    Windows = try(winMenuDel(menuname), silent = TRUE),
    Darwin = .mac_menu_del(menuname),
    .unix_menu_del(menuname)
  )
  invisible(res)
}

#' @export
#' @rdname menu
menuDel <- menu_del # Backward compatibility

#' @export
#' @rdname menu
menu_del_item <- function(menuname, itemname) {
  menuname <- .check_menu_name(menuname)

  if (.is_jgr()) return(invisible(.jgr_menu_del_item(menuname, itemname)))
  res <- switch(Sys.info()["sysname"],
    Windows = try(winMenuDelItem(menuname, itemname), silent = TRUE),
    Darwin = .mac_menu_del_item(menuname, itemname),
    .unix_menu_del_item(menuname, itemname)
  )
  invisible(res)
}

#' @export
#' @rdname menu
menuDelItem <- menu_del_item # Backward compatibility


# Hidden enu functions
.menu_clear <- function() {
  if (.is_jgr()) return(invisible(NULL))
  res <- switch(Sys.info()["sysname"],
    Windows = NULL,
    Darwin = .mac_menu_clear(),
    .unix_menu_clear()
  )
  invisible(res)
}

.menu_file_init <- function() {
  if (.is_jgr()) return(invisible(NULL))
  res <- switch(Sys.info()["sysname"],
    Windows = NULL,
    Darwin = NULL, # TODO: should we have a default menu?
    .unix_menu_file_init()
  )
  invisible(res)
}

.ctx_menu_file_init <- function() {
  res <- switch(Sys.info()["sysname"],
    Windows = NULL,
    Darwin = NULL, # TODO: should we have a default menu?
    .unix_ctx_menu_file_init()
  )
  invisible(res)
}

.check_menu_name <- function(menuname) {
  # Make sure menuname is correct...
  menuname <- as.character(menuname)
  if (length(menuname) != 1)
    stop("'menuname' must be a single character string")

  # TODO: this is not allowed in JGR!
  # $ConsoleMain/<menu> is equivalent, and thus, transformed into <menu>
  menuname <- sub("^\\$ConsoleMain/", "", menuname)

  if (menuname == "")
    stop("You cannot use an empty menuname")
  # Do not accept $ConsoleMain, $ConsolePopup, $Graph<n>Main,
  # or $Graph<n>Popup alone: need a / after the name of a special menu
  if (grepl("^\\$Console(Main|Popup)", menuname) &&
      !grepl("^\\$Console(Main|Popup)/.+", menuname))
    stop("You must define a submenu after the name of a special menu")
  if (grepl("^\\$Graph[0-9]+(Main|Popup)", menuname) &&
      !grepl("^\\$Graph[0-9]+(Main|Popup)/.+", menuname))
    stop("You must define a submenu after the name of a special menu")
  # Return the (possibly arranged) menuname
  menuname
}


# JGR menus manipulation functions
# The default menus in JGR (for consistence with other implementations, we
# don't want to see them!)
.jgr_default_menus <-
  c("File", "Edit", "Workspace", "Packages & Data", "Window", "Help")

.jgr_menu_mem <- function() {
  # Get an environment with info about JGR menus I cannot get otherwise
  # Basically, I keep track of two things here:
  # 1) which menu action is related to which menu entry
  # 2) for separators, names can be any number of '-', but it is only '-'
  #    in JGR => keep track of the correspondance!
  mnu <- .get_temp(".jgr_menu_mem")
  if (is.null(mnu)) {
    mnu <- new.env()
    .assign_temp(".jgr_menu_mem", mnu)
  }
  mnu
}

.jgr_menu_mem_add <- function(menuname, itemname, action) {
  e <- .jgr_menu_mem()
  e[[paste(menuname, itemname, sep = "//")]] <- action
}

.jgr_menu_mem_get <- function(menuname, itemname) {
  e <- .jgr_menu_mem()
  e[[paste(menuname, itemname, sep = "//")]]
}

.jgr_menu_mem_del <- function(menuname, itemname) {
  e <- .jgr_menu_mem()
  item <- paste(menuname, itemname, sep = "//")
  if (exists(item, envir = e, inherits = FALSE)) rm(list = item, envir = e)
}

# I redefine the jgr.XXX() function here because I don't want to depend
# on JGR in this package (too much trouble on install of JGR and I never
# want to force users of svDialogs to install JGR)
.jgr.register.function <- function(fun) {
  if (is.null(.GlobalEnv$.jgr.user.functions))
    .GlobalEnv$.jgr.user.functions <- list()
  fnc <- length(.GlobalEnv$.jgr.user.functions) + 1
  .GlobalEnv$.jgr.user.functions[[fnc]] <- fun
  paste(".jgr.user.functions[[", fnc, "]]()", sep = "")
}

.jgr.get_menu_names <- function() {
  if (!.is_jgr()) {
    cat(".jgr.get_menu_names() cannot be used outside JGR.\n")
    return(invisible(NULL))
	  J <- function(...) return() # Just to avoir R CMD check warning
  }
  J("org/rosuda/JGR/JGR")$getMenuNames()
}

.jgr.get_menu_item_names <- function(menu) {
  if (!.is_jgr()) {
    cat("/jgr.get_menu_item_names() cannot be used outside JGR.\n")
    return(invisible(NULL))
    J <- function(...) return() # Just to avoir R CMD check warning
  }
  J("org/rosuda/JGR/JGR")$getMenuItemNames(as.character(menu))
}

.jgr.add_menu <- function(name) {
  if (!.is_jgr()) {
    cat(".jgr.add_menu() cannot be used outside JGR.\n")
    return(invisible(NULL))
    .jcall <- function(...) return() # Just to avoir R CMD check warning
  }
  invisible(.jcall("org/rosuda/JGR/JGR", "V", "addMenu", as.character(name)))
}

.jgr.insert_menu <- function(name, index) {
  if (!.is_jgr()) {
    cat(".jgr.insert_menu() cannot be used outside JGR.\n")
    return(invisible(NULL))
    .jcall <- function(...) return() # Just to avoir R CMD check warning
  }
  invisible(.jcall("org/rosuda/JGR/JGR", "V", "insertMenu",
    as.character(name), as.integer(index - 1)))
}

.jgr.add_menu_item <- function(menu, name, command, silent = TRUE) {
  if (!.is_jgr()) {
    cat(".jgr.add_menu_item() cannot be used outside JGR.\n")
    return(invisible(NULL))
    .jcall <- function(...) return() # Just to avoir R CMD check warning
  }
  if (is.function(command))
    command <- .jgr.register.function(command)
  invisible(.jcall("org/rosuda/JGR/JGR", "V", "addMenuItem",
    as.character(menu), as.character(name), as.character(command),
    as.logical(silent)))
}

.jgr.insert_menu_item <- function(menu, name, command, index, silent = TRUE) {
  if (!.is_jgr()) {
    cat(".jgr.insert_menu_item() cannot be used outside JGR.\n")
    return(invisible(NULL))
    .jcall <- function(...) return() # Just to avoir R CMD check warning
  }
  if (is.function(command))
    command <- .jgr.register.function(command)
  invisible(.jcall("org/rosuda/JGR/JGR", "V", "insertMenuItem",
    as.character(menu), as.character(name), as.character(command),
    as.logical(silent), as.integer(index - 1)))
}

.jgr.add_menu_separator <- function(menu) {
  if (!.is_jgr()) {
    cat(".jgr.add_menu_separator() cannot be used outside JGR.\n")
    return(invisible(NULL))
    .jcall <- function(...) return() # Just to avoir R CMD check warning
  }
  invisible(.jcall("org/rosuda/JGR/JGR", "V", "addMenuSeparator",
    as.character(menu)))
}

.jgr.insert_menu_separator <- function(menu, index) {
  if (!.is_jgr()) {
    cat(".jgr.insert_menu_separator() cannot be used outside JGR.\n")
    return(invisible(NULL))
    .jcall <- function(...) return() # Just to avoir R CMD check warning
  }
  invisible(.jcall("org/rosuda/JGR/JGR", "V", "insertMenuSeparator",
    as.character(menu), as.integer(index - 1)))
}

.jgr.add_sub_menu <- function(menu, subMenuName, labels, commands) {
  if (!.is_jgr()) {
    cat(".jgr.add_sub_menu() cannot be used outside JGR.\n")
    return(invisible(NULL))
    J <- function(...) return() # Just to avoir R CMD check warning
  }
  invisible(J("org/rosuda/JGR/JGR")$addSubMenu(menu, subMenuName,
    labels, commands))
}

# There seems to be a bug in the original jgr.insertSubMenu() function
.jgr.insert_sub_menu <- function(menu, subMenuName, labels, commands, index) {
  if (!.is_jgr()) {
    cat(".jgr.add_sub_menu() cannot be used outside JGR.\n")
    return(invisible(NULL))
    J <- function(...) return() # Just to avoir R CMD check warning
  }
  invisible(J("org/rosuda/JGR/JGR")$insertSubMenu(menu, subMenuName,
    as.integer(index - 1), labels, commands))
}

.jgr.remove_menu <- function(index) {
  if (!.is_jgr()) {
    cat(".jgr.remove_menu() cannot be used outside JGR.\n")
    return(invisible(NULL))
    J <- function(...) return() # Just to avoir R CMD check warning
  }
  J("org/rosuda/JGR/JGR")$removeMenu(as.integer(index - 1))
}

.jgr.remove_menu_item <- function(menu, index) {
  if (!.is_jgr()) {
    cat(".jgr.remove_menu_item() cannot be used outside JGR.\n")
    return(invisible(NULL))
    J <- function(...) return() # Just to avoir R CMD check warning
  }
  J("org/rosuda/JGR/JGR")$removeMenuItem(as.character(menu),
    as.integer(index - 1))
}

.jgr_run <- function(cmd, envir = .GlobalEnv) {
  # This function is used for submenus where it is not currently possible
  # to declare silent = FALSE
  # Basically, it prints the command, then, evaluate it, capturing output
  # and finally, it prints that output
  cat("> ", deparse(substitute(cmd)), "\n", sep = "")
  res <- capture.output(cmd)
  cat(res)
  cat("\n")
}

# Prepare a command to be printed more or less correctly on screen
# for JGR submenus
.jgr_action <- function(cmd) paste0("svDialogs:::.jgr_run(", cmd, ")")

# Implementation of JGR menus manipulation
.jgr_menu_names <- function() {
  # For consistency with the other implementations, do not return
  # the stadard menus File, Edit, Workspace, Packages & Data, Window, Help
  res <- .jgr.get_menu_names()
  res[!res %in% .jgr_default_menus]
}

.jgr_menu_items <- function(menuname) {
  # For consistency with the other implementations, return character(0)
  res <- try(.jgr.get_menu_item_names(menuname), silent = TRUE)
  # if the menu is not found
  if (inherits(res, "try-error")) {
    character(0)
  } else {
    res
  }
}

.jgr_menu_add <- function(menuname) {
  # In JGR, one accepts only menus and one level of submenus... and
  # submenus are managed in a quite different way!
  # For compatibility, we allow to use menu/submenu
  mnu <- strsplit(menuname[1], "/", fixed = TRUE)[[1]]
  l <- length(mnu)
  if (l == 0) return(invisible(NULL))

  .add_top_menu <- function(topmenu) {
    if (!topmenu %in% .jgr.get_menu_names()) # Here, we check all JGR menus!
    .jgr.add_menu(topmenu)
  }

  if (l == 1) {
    .add_top_menu(mnu)
    return(invisible(NULL))
  }
  if (l > 2) stop("Only one submenu level allowed on JGR")
  # Make sure the topmenu is define, and add an empty submenu to it
  .add_top_menu(mnu[1])
  .jgr.add_sub_menu(mnu[1], mnu[2], character(0), character(0))
  invisible(NULL)
}

.jgr_menu_add_item <- function(menuname, itemname, action) {
  # In JGR, one accepts only menus and one level of submenus... and
  # submenus are managed in a quite different way!
  # For compatibility, we allow to use menu/submenu
  mnu <- strsplit(menuname[1], "/", fixed = TRUE)[[1]]
  l <- length(mnu)
  if (l == 0) return(invisible(NULL)) # Nothing to do...

  silent <- FALSE
  # Special cases for "none", no action associated with this menu
  if (action == "none") {
    action <- ""
    silent <- TRUE
  }
  # Replace \n by \\n, and \t by \\t
  action <- gsub("\n", "\\\\n", action)
  action <- gsub("\t", "\\\\t", action)

  if (l == 1) {
    # First, make sure the menu exists
    .jgr_menu_add(mnu)
    # Are we trying to add a separator?
    if (grepl("^-+$", itemname)) { # This must be a separator
      .jgr.add_menu_separator(mnu)
    } else {# This must be a menu entry
      # Is the menu entry already implemented?
      items <- .jgr_menu_items(mnu)
      if (itemname %in% items) {
        # Delete and recreate it with the new action
        idx <- (seq_along(items))[items == itemname][1]
        .jgr.remove_menu_item(mnu, idx)
        if (action == "enable")
          action <- .jgr_menu_mem_get(mnu, itemname)
        if (is.null(action)) action <- ""
        if (action == "disable") {
          action <- 'cat("- disabled menu item...\n")'
          silent <- TRUE
          .jgr.insert_menu_item(mnu, itemname, action, idx, silent = silent)
          # Don't change action in .jgr_menus() so that we can
          # recover it with "enable"!
        } else {
          .jgr.insert_menu_item(mnu, itemname, action, idx, silent = silent)
          .jgr_menu_mem_add(mnu, itemname, action)
        }
      } else {
        # This is a new item => just add it
        if (action == "enable") action <- ""
        if (action == "disable") {
          action <- 'cat("- disabled menu item...\n")'
          silent <- TRUE
          .jgr.add_menu_item(mnu, itemname, action, silent = silent)
          .jgr_menu_mem_add(mnu, itemname, "")
        } else {
          .jgr.add_menu_item(mnu, itemname, action, silent = silent)
          .jgr_menu_mem_add(mnu, itemname, action)
        }
      }
    }
    return(invisible(NULL))
  }

  if (l == 2) {
    # We add an entry in a submenu. In JGR, we must delete and
    # reconstruct the submenu entirely!
    # Does this submenu already exists?
    # (note: in JGR it can be a menu entry as well!)
    items <- .jgr_menu_items(mnu[1])
    if (mnu[2] %in% items) { # The submenu already exists...
      # Delete it and reconstruct it with the added or changed item
      idx <- (seq_along(items))[items == mnu[2]][1]
      .jgr.remove_menu_item(mnu[1], idx)
      # Get the list of entries in the submenu from .jgr_menus
      # (how to get it otherwise???)
      actions <- .jgr_menu_mem_get(mnu[1], mnu[2])
      if (is.null(actions)) {# Apparently nothing in there yet
        if (action == "enable") action <- ""
        if (action == "disable") {
          action <- 'cat("- disabled menu item...\n")'
          .jgr.insert_sub_menu(mnu[1], mnu[2], c(itemname, "-"),
            c(action, ""), idx)
          actions <- ""
        } else {
          action <- .jgr_action(action)
          .jgr.insert_sub_menu(mnu[1], mnu[2], c(itemname, "-"),
            c(action, ""), idx)
          actions <- action
        }
        names(actions) <- itemname
        .jgr_menu_mem_add(mnu[1], mnu[2], actions)
      } else {# There are already items in this submenu
        if (action == "disable") { # We want to disable one action
          if (!itemname %in% names(actions))
            return(invisible(NULL))
          actions[[itemname]] <- 'cat("- disabled menu item...\n")'
          if (length(actions) == 1) {
            .jgr.insert_sub_menu(mnu[1], mnu[2],
              c(names(actions), "-"), c(actions, ""), idx)
          } else {
            .jgr.insert_sub_menu(mnu[1], mnu[2],
              names(actions), actions, idx)
          }
        } else if (action == "enable") {
          if (!itemname %in% names(actions))
            return(invisible(NULL))
          if (length(actions) == 1) {
            .jgr.insert_sub_menu(mnu[1], mnu[2],
              c(names(actions), "-"), c(actions, ""), idx)
          } else {
            .jgr.insert_sub_menu(mnu[1], mnu[2],
              names(actions), actions, idx)
          }
        } else {# An action is defined
          actions[[itemname]] <- .jgr_action(action)
          if (length(actions) == 1) {
            .jgr.insert_sub_menu(mnu[1], mnu[2],
              c(names(actions), "-"), c(actions, ""), idx)
          } else {
            .jgr.insert_sub_menu(mnu[1], mnu[2],
              names(actions), actions, idx)
          }
          .jgr_menu_mem_add(mnu[1], mnu[2], actions)
        }
      }
    } else {# The submenu does not exists yet, create it now
      # First, make sure the top menu exists
      .jgr_menu_add(mnu[1])
      if (action == "enable") action <- ""
      if (action == "disable") {
        action <- 'cat("- disabled menu item...\n")'
        .jgr.add_sub_menu(mnu[1], mnu[2], c(itemname, "-"), c(action, ""))
        actions <- ""
      } else {
        action <- .jgr_action(action)
        .jgr.add_sub_menu(mnu[1], mnu[2], c(itemname, "-"), c(action, ""))
        actions <- action
      }
      names(actions) <- itemname
      .jgr_menu_mem_add(mnu[1], mnu[2], actions)
    }
    return(invisible(NULL))
  }

  if (l > 2)
    stop("Only one submenu level allowed on JGR")
}

.jgr_menu_del <- function(menuname) {
  # In JGR, one accepts only menus and one level of submenus... and
  # submenus are managed in a quite different way!
  # For compatibility, we allow to use menu/submenu
  mnu <- strsplit(menuname[1], "/", fixed = TRUE)[[1]]
  l <- length(mnu)
  if (l == 0)
    return(invisible(NULL))

  if (l == 1) {
    if (mnu %in% .jgr_default_menus) # Do not allow to delete default menus
      return(invisible(NULL))
    # Get the position of this menu and make sure it is not a default menu!
    allmnu <- .jgr.get_menu_names()
    allpos <- seq_along(allmnu)
    pos <- allpos[allmnu == mnu]
    if (!length(pos))
      return(invisible(NULL)) # Not found
    pos <- rev(pos)[1]
    .jgr.remove_menu(pos)
  }
  if (l == 2) {
    # Remove a submenu (note for JGR, this is the same as removing a menu item!)
    .jgr_menu_del_item(mnu[1], mnu[2])
  }
  # If there are more levels, do nothing because these submenus do not exist!
  invisible(NULL)
}

.jgr_menu_del_item <- function(menuname, itemname) {
  # On JGR, all separators are named "-", but on, e.g., Windows, I must
  # use a different name for each separator => What should we do???
  # Here, we consider '-' for the first one, '--' for the second one, etc.
  mnu <- strsplit(menuname[1], "/", fixed = TRUE)[[1]]
  l <- length(mnu)
  if (l == 0)
    return(invisible(NULL))

  if (l == 1) {
    # Are we trying to delete a separator?
    items <- .jgr_menu_items(mnu)
    if (grepl("^-+$", itemname)) { # This must be a separator
      is_sep <- items == "-"
      pos_sep <- (seq_along(items))[is_sep]
      # Depending on the number of minus signs we try to remove
      # first, second, etc. separator
      n_sep <- nchar(itemname)
      if (length(pos_sep) < n_sep)
        return(invisible(NULL))
      idx <- pos_sep[n_sep]
    } else {# This must be a menu entry
      idx <- (seq_along(items))[itemname == items]
    }
    if (!length(idx))
      return(invisible(NULL))
    .jgr.remove_menu_item(mnu, idx)
    .jgr_menu_mem_del(mnu, itemname)
  }

  if (l == 2) {
    # We want to eliminate an item from a submenu. In JGR, there is no
    # function for that, but we can delete and recreate the submenu
    # without this item!
    items <- .jgr_menu_items(mnu[1])
    if (!mnu[2] %in% items) return(invisible(NULL)) # Submenu not there
    # Get the list of submenus currently defined from our cache version
    actions <- .jgr_menu_mem_get(mnu[1], mnu[2])
    if (is.null(actions) || !itemname %in% names(actions))
      return(invisible(NULL)) # Apparently not there
    # Delete and reconstruct the submenu without itemname
    idx <- (seq_along(items))[items == mnu[2]][1]
    .jgr.remove_menu_item(mnu[1], idx)
    # Recreate the submenu after eliminating itemname
    actions <- actions[names(actions) != itemname]
    if (!length(actions)) {
      .jgr.insert_sub_menu(mnu[1], mnu[2], character(0), character(0), idx)
    } else if (length(actions) == 1) {
      .jgr.insert_sub_menu(mnu[1], mnu[2], c(names(actions), "-"),
        c(actions, ""), idx)
  } else {
    .jgr.insert_sub_menu(mnu[1], mnu[2], names(actions), actions, idx)
    }
    .jgr_menu_mem_add(mnu[1], mnu[2], actions)
}
  invisible(NULL) # No more than one submenu level allowed!
}


# Windows version and standard winMenuXXX
# TODO: fallback system for Rterm???
.winMenuAddItem <- function(menuname, itemname, action) {
  # As in R 2.14.1, the original winMenuAddItem() does things I don't like
  # much when using 'enable' or 'disable' for action, on a non existing menu:
  # it creates it with the action being 'enable' or 'disable'... I suppose
  # if is a feature, but I want a different behaviour here: to ignore such
  # a command applied to a non-existing menu item!
  if (action %in% c("enable", "disable")) {
    menus <- winMenuItems(menuname)
    if (!is.null(menus) && itemname %in% names(menus)) {
      # The menu exists... enable or disable it!
      return(winMenuAddItem(menuname, itemname, action))
    } else return(invisible(NULL))
  } else return(winMenuAddItem(menuname, itemname, action))
}


# MacOS version of menus
# Note, either we use AppleScript folder (by default) or the XMenu folder
# Install XMenu from http://xmenu.en.softonic.com/mac.
# You should use the custom commands only for R, because it will erase
# everything in it everytime the svDialogs package starts!
# Configure XMenu to display only User-Defined items, and name it R.
# Select folders before files, for icons: None, Big font
# and for menu titles: Text and then, Start at login
#options(useXMenu = TRUE)
.mac_menu_folder <- function() {
  # Get the root folder for the R menus, depends on wether we use XMenu or not
  use_xmenu <- getOption("useXMenu", default = NULL)
  if (is.null(use_xmenu)) {
    # If not specified, look if a "R" file or folder exists
    use_xmenu <- file.exists("~/Library/Application Support/XMenu/R")
  } else use_xmenu <- isTRUE(use_xmenu)
  getOption("menuFolder", default = if (use_xmenu)
    "~/Library/Application Support/XMenu/Custom" else
    "~/Library/Scripts/Applications/R")
}

.mac_menu_clear <- function() {
  #stop("Not implemented yet!")

#  # To be called when svDialogs package loads: make sure to zap all
#  # custom menu items that may have been previously defined
#  # (also call it when the package closes)
#  odir <- getwd()
#  on.exit(setwd(odir))
#  setwd(.macMenuFolder())
#  setwd("..")
#  folder <- file.path(".", basename(.macMenuFolder()))
#  unlink(folder, recursive = TRUE)
#  dir.create(folder, recursive = TRUE)
#  # Now, I can assume that the dir is created and is empty
#  invisible(NULL)
}

.mac_menu_names <- function() {
  #stop("Not implemented yet!")
}

.mac_menu_items <- function(menuname) {
  #stop("Not implemented yet!")
}

.mac_menu_add <- function(menuname) {
  #stop("Not implemented yet!")

#  # Menus are folders created in ~/Scripts/Applications/R/Custom
#  # I just need to create (recursively) the directories
#  dir.create(file.path(.macMenuFolder(), menuname),
#    showWarnings = FALSE, recursive = TRUE)
#  invisible(NULL)
}

.mac_menu_add_item <- function(menuname, itemname, action) {
  #stop("Not implemented yet!")

#  # TODO: manage 'enable' and 'disable'!!!
#  # Make sure that the dir is created
#  .mac_menu_add(menuname)
#  # Switch to this folder
#  odir <- getwd()
#  on.exit(setwd(odir))
#  setwd(file.path(.mac_menu_folder(), menuname))
#  # Add an executable file in it with 'itemname' name
#  # that contains AppleScript code to run action in R
#  # Determine if R is run in R.app or in a terminal window
#  if (.Platform$GUI == "AQUA") {
#    # Can be R or R64 or SciViews R or SciViews R64!
#    app <- paste0('"', system("osascript -e 'name of application \"R\"'",
#      intern = TRUE), '"')
#  } else app <- "\"Terminal\""
#  # Define action accordingly
#  if (action == "none") {
#    cmd <- "to activate"
#  } else {
#    # Make sure to quote "
#    action <- gsub('"', '\\\\"', action)
#    # Also replace \n, \r and \t
#    action <- gsub('\n', '\\\\\\\\n', action)
#    action <- gsub('\r', '\\\\\\\\r', action)
#    action <- gsub('\t', '\\\\\\\\t', action)
#    if (app == "\"Terminal\"") {
#      cmd <- paste("to do script \"", action, "\" in window 1", sep = "")
#    } else {
#      cmd <- paste("to cmd \"", action, "\"", sep = "")
#    }
#  }
#  # Compile applescript item
#  system(paste("osacompile -e 'tell application ", app, " ", cmd,
#    "' -o \"", itemname, ".app\"", sep = ""), ignore.stdout = TRUE,
#    ignore.stderr = TRUE)
#  invisible(NULL)
}

.mac_menu_del <- function(menuname) {
  #stop("Not implemented yet!")

#  # Unlink does not like ~ => change working dir first
#  odir <- getwd()
#  on.exit(setwd(odir))
#  setwd(.mac_menu_folder())
#  unlink(menuname, recursive = TRUE)
#  invisible(NULL)
}

.mac_menu_del_item <- function(menuname, itemname) {
  #stop("Not implemented yet!")

#  # Unlink does not like ~ => change working dir first
#  odir <- getwd()
#  on.exit(setwd(odir))
#  setwd(file.path(.mac_menu_folder()))
#  unlink(file.path(".", menuname, paste(itemname, "app", sep = ".")),
#    recursive = TRUE)
#  invisible(NULL)
}


# This holds the custom menu structure in an R object
.Rmenu <- function() {
  # The custom R menu is cached in a Rmenu object in SciViews:TempEnv
  .get_temp("Rmenu", default = list(), mode = "list")
}


# Linux/Unix version
# To use R custom context menu, you have to install xvkbd, xdotool,
# zenity (and, possibly, yad)
# You need also to compile and install ctxmenu
# On Ubuntu:
# sudo apt-get install xvkbd xdotool zenity
# Warning, you need to install the English (US) keymap, even if you don't use
# it. Otherwise, xvkbd will issue strange things in your R console!
# TODO: install and configure ctxmenu... + add shortcut keys!
# Use xbindkeys to bind shell commands to keyboard and mouse keys
# chmod +x ctxmenu

# We need to write menu files in /tmp... but CRAN policies do not allow this
# unless we got acknowledgement by the user in the interactive session.
# This function checks that R runs in interactive mode and the user gave
# acknowledgement, either interactively,  or by defining the option
# 'svDialogs.tmpfiles' to TRUE
.tmpfiles_allowed <- function()
  interactive() && isTRUE(getOption("svDialogs.tmpfiles", FALSE))

.unix_tmpfiles_ask <- function() {
  if (!interactive()) return(FALSE)

  # Make sure the user gave explicit right to create custom menus temp
  # files, either interactively, or through the setting of an option
  # options(svDialogs.tmpfiles = TRUE)
  opt <- getOption("svDialogs.tmpfiles")
  if (is.null(opt)) {# Ask user interactively
    if (ok_cancel_box(
      "Install custom menu configuration files in ~/.ctxmenu/tmp/?")) {
      options(svDialogs.tmpfiles = TRUE)
      # Make sure to clear old menus, and to install new ones
      .menu_clear()
      .menu_file_init()
      .ctx_menu_file_init()
      # Make sure that ctxmenu is installed
      if (Sys.which("ctxmenu") == "") {
        warning("Menus will not be displayed if you do not install ctxmenu",
          " properly, see: http://www.sciviews.org/SciViews-R/ctxmenu.zip")
        return(FALSE)
      } else return(TRUE) # Everything should be ok!
    } else {
      options(svDialogs.tmpfiles = FALSE)
      # Indicate that it will not be possible to use custom menus before
      # allowed by the corresponding option
      warning("Menus will not be displayed unless you agree to create config",
        " files using options(svDialogs.tmpfiles = TRUE)")
      return(FALSE)
    }
  } else if (!isTRUE(opt)) {
    warning("Menus will not be displayed unless you agree to create config",
      " files using options(svDialogs.tmpfiles = TRUE)")
    return(FALSE)
  } else return(TRUE) # Note: we do not check again for ctxmenu here!
}

.unix_menu_folder <- function() {
  # Get the root folder for the R menus
  mnu_dir <- getOption("menuFolder", default = "~/.ctxmenu/tmp")
  # Make sure this directory exists, in one can write to it!
  if (.tmpfiles_allowed() && !file.exists(mnu_dir))
    try(dir.create(mnu_dir, showWarnings = FALSE, recursive = TRUE),
      silent = TRUE)
  mnu_dir
}

.unix_menu_file <- function() {
  # Get the name of the file that contains the R menu
  winid <- .get_temp(".winid", default = Sys.getenv("WINDOWID"))
  .assign_temp(".winid", winid)
  # Do not use user name in the filename (B. Ripley's request)
  #user <- .get_temp(".user", default = Sys.getenv("USER"))
  #.assign_temp(".user", user)
  #file.path(.unix_menu_folder(), paste0(user, winid, "Menu.txt"))
  file.path(.unix_menu_folder(), paste0(winid, "Menu.txt"))
}

.unix_menu_file_init <- function() {
  # Can we generate files in the temporary directory?
  if (!.tmpfiles_allowed())
    return(invisible(NULL))

  # Initialize the R menu file with default items
  fil <- .unix_menu_file()
  # Get the default R menu and start from there
  def <- getOption("RMenuFile",
    default = file.path("~", ".ctxmenu", "RMenu.txt"))
  if (file.exists(def)) {
    file.copy(def, fil, overwrite = TRUE)
  } else file.copy(system.file("gui", "RMenuLinux.txt",
    package = "svDialogs"), fil, overwrite = TRUE)
  invisible(NULL)
}

.unix_ctx_menu_file <- function() {
  # Get the name of the file that contains the R context menu
  winid <- .get_temp(".winid", default = Sys.getenv("WINDOWID"))
  .assign_temp(".winid", winid)
  # Do not use user name in the filename (B. Ripley's request)
  #user <- .get_temp(".user", default = Sys.getenv("USER"))
  #.assign_temp(".user", user)
  #file.path(.unix_menu_folder(), paste0(user, winid, "CtxMenu.txt"))
  file.path(.unix_menu_folder(), paste0(winid, "CtxMenu.txt"))
}

.unix_ctx_menu_file_init <- function() {
  # Can we generate files in the temporary directory?
  if (!.tmpfiles_allowed()) return(invisible(NULL))

  # Initialize the R context menu file with default items
  fil <- .unix_ctx_menu_file()
  # Get the default R context menu and start from there
  def <- getOption("RCtxMenuFile",
    default = file.path("~", ".ctxmenu", "RCtxMenu.txt"))
  if (file.exists(def)) {
    file.copy(def, fil, overwrite = TRUE)
  } else file.copy(system.file("gui", "RCtxMenuLinux.txt",
    package = "svDialogs"), fil, overwrite = TRUE)
  invisible(NULL)
}

.unix_menu_save <- function(mnu, file = TRUE) {
  # Save the menu structure in Rmenu object in SciViews:TempEnv and in a file
  # mnu is either a list of lists with menu entries, or NULL to delete all
  # custom menus
  .assign_temp("Rmenu", mnu)

  # Do nothing on files, unless interactive() and got user's acceptation
  if (!.tmpfiles_allowed()) return(invisible(NULL))
  if (!isTRUE(file)) return(invisible(NULL))
  # The menu file is:
  fil <- .unix_menu_file()
  ctx_fil <- .unix_ctx_menu_file()
  if (is.null(mnu)) {
    # Clear the file
    unlink(fil)
  } else {
    # Populate the file with the content of the Rmenu object
    make_menu <- function(lst, indent = 0, file = fil, ctxfile = ctx_fil) {
      l <- length(lst)
      if (l < 1) return()
      nms <- names(lst)
      for (i in 1:l) {
        item <- nms[i]
        if (is.list(lst[[i]])) {
          # Special case for '$ConsolePopup'
          if (item == "$ConsolePopup" && !is.null(ctxfile)) {
            make_menu(lst[[i]], indent = 0, file = ctxfile, ctxfile = NULL)
          } else {
            # Create a new menu
            cat("\n", rep("\t", indent), "submenu=", item, "\n",
              sep = "", file = file, append = TRUE)
            make_menu(lst[[i]], indent = indent + 1, file = file,
              ctxfile = NULL)
          }
        } else {
          # Is this a separator?
          if (grepl("^-",  item)) {
            cat("\n", rep("\t", indent), "separator\n",
              sep = "", file = file, append = TRUE)
          } else {# Add an item in current menu
            ind <- rep("\t", indent)
            # Rework commands using xvkbd -xsendevent -text "cmd\r"
            cmd <- as.character(lst[[i]])[1]
            if (cmd == "none" || !is.null(attr(lst[[i]], "state"))) {
              cmd <- "NULL" # This is the "no cmd" or "disabled" for ctxmenu
            } else {
              cmd <- paste(cmd, "\\n", sep = "")
              cmd <- paste("xvkbd -xsendevent -text", shQuote(cmd))
            }
            cat("\n", ind, "item=", item, "\n", ind, "cmd=", cmd,
              "\n", sep = "", file = file, append = TRUE)
          }
        }
      }
    }
    # Initialize the menu with default items
    .unix_menu_file_init()
    .unix_ctx_menu_file_init()
    # Add custom menus to it...
    cat("\nseparator # Here starts the custom R menus\n\n",
      file = fil, append = TRUE)
    cat("\nseparator # Here starts the custom R context menus\n\n",
      file = ctx_fil, append = TRUE)
    make_menu(mnu)
  }
  invisible(fil)
}

.unix_menu_clear <- function() {
  # To be called when svDialogs package loads: make sure to zap all
  # custom menu items that may have been previously defined
  # (also call it when the package closes)

  # Do nothing on files, unless interactive() and got user's acceptation
  if (!.tmpfiles_allowed()) return(invisible(NULL))

  # Also clear the local object
  .unix_menu_save(NULL)
  unlink(.unix_menu_file())
  unlink(.unix_ctx_menu_file())
  invisible(NULL)
}

.unix_menu_names <- function(mnu = .Rmenu(), parent = character(0)) {
  if (!length(mnu)) return(character(0))
  # List all custom menu and (sub)menu
  # Iteratively traverse the list and enumerate all sublists (= submenus)
  items <- names(mnu)
  submnus <- character(0)
  for (item in items)
    if (is.list(mnu[[item]]))
      submnus <- c(submnus, paste0(parent, item),
        .unix_menu_names(mnu[[item]], parent = paste0(parent, item, "/")))
  names(submnus) <- NULL
  # Eliminate $ConsolePopup, $Graph<n>Main & $Graph<n>Popup
  submnus <- submnus[submnus != "$ConsolePopup"]
  isGraphMenu <- grepl("^\\$Graph[0-9]+(Main|Popup)$", submnus)
  submnus <- submnus[!isGraphMenu]
  submnus
}

.unix_menu_items <- function(menuname) {
  # List all menu items in a given (sub)menu
  mnu <- .Rmenu()
  items <- strsplit(as.character(menuname), "/", fixed = TRUE)[[1]]
  # Traverse the menu hierarchy
  for (i in items) {
    mnu <- mnu[[i]]
    if (is.null(mnu) || !is.list(mnu))
      stop("unable to retrieve items for ", menuname,
        " (menu does not exist)")
  }
  if (length(mnu) == 0) return(character(0))
  # Set all submenu items to NULL
  for (i in seq_along(mnu))
    if (is.list(mnu[[i]])) mnu[[i]] <- NULL
  if (length(mnu) == 0) {
    character(0)
  } else {
    unlist(mnu)
  }
}

.unix_menu_add <- function(menuname, itemname = NULL, action = "none") {
  # Make sure we can install required files in ~/.ctxmenu/tmp/
  .unix_tmpfiles_ask()

  # Add this menu to our Rmenu object
  mnu <- .Rmenu()
  items <- strsplit(as.character(menuname), "/", fixed = TRUE)[[1]]
  # Allow for a maximum of 5 sublevels (should be largely enough!)
  l <- length(items)
  if (l == 1) {
    if (!is.null(mnu[[items[1]]])) {
      # If this is not a list, we got an error
      if (!is.list(mnu[[items[1]]]))
        stop(menuname, " is already defined and is not a menu")
    } else {# Create it
      mnu[[items[1]]] <- list()
    }
    # Do we create an menu item there too?
    if (!is.null(itemname))
      mnu[[items[1]]][[itemname]] <- action
  } else if (l == 2) {
    if (!is.null(mnu[[items[1]]][[items[2]]])) {
      # If this is not a list, we got an error
      if (!is.list(mnu[[items[1]]][[items[2]]]))
        stop(menuname, " is already defined and is not a menu")
    } else {# Create it
      mnu[[items[1]]][[items[2]]] <- list()
    }
    # Do we create an menu item there too?
    if (!is.null(itemname))
      mnu[[items[1]]][[items[2]]][[itemname]] <- action
  } else if (l == 3) {
    if (!is.null(mnu[[items[1]]][[items[2]]][[items[3]]])) {
      # If this is not a list, we got an error
      if (!is.list(mnu[[items[1]]][[items[2]]][[items[3]]]))
        stop(menuname, " is already defined and is not a menu")
    } else {# Create it
      mnu[[items[1]]][[items[2]]][[items[3]]] <- list()
    }
    # Do we create an menu item there too?
    if (!is.null(itemname))
      mnu[[items[1]]][[items[2]]][[items[3]]][[itemname]] <- action
  } else if (l == 4) {
    if (!is.null(mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]])) {
      # If this is not a list, we got an error
      if (!is.list(mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]]))
        stop(menuname, " is already defined and is not a menu")
    } else {# Create it
      mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]] <- list()
    }
    # Do we create an menu item there too?
    if (!is.null(itemname))
      mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[itemname]] <- action
  } else if (l == 5) {
    if (!is.null(
      mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[items[5]]])) {
      # If this is not a list, we got an error
      if (!is.list(
        mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[items[5]]]))
        stop(menuname, " is already defined and is not a menu")
    } else {# Create it
      mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[items[5]]] <- list()
    }
    # Do we create an menu item there too?
    if (!is.null(itemname))
      mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[
        items[5]]][[itemname]] <- action
  } else if (l > 5) {
    stop("You cannot use more than 5 menu levels")
  }
  # Save these changes
  .unix_menu_save(mnu)
  invisible(NULL)
}

.unix_menu_add_item <- function(menuname, itemname, action) {
  # Make sure we can install required files in ~/.ctxmenu/tmp/
  .unix_tmpfiles_ask()

  if (action %in% c("enable", "disable")) {
    # Enable or disable an existing menu item
    if (action == "enable") action <- NULL # To eliminate the attribute
    mnu <- .Rmenu()
    items <- strsplit(as.character(menuname), "/", fixed = TRUE)[[1]]
    # Allow for a maximum of 5 sublevels (should be largely enough!)
    l <- length(items)
    if (l == 1) {
      if (!is.null(mnu[[items[1]]][[itemname]]))
        attr(mnu[[items[1]]][[itemname]], "state") <- action
    } else if (l == 2) {
      if (!is.null(mnu[[items[1]]][[items[2]]][[itemname]]))
        attr(mnu[[items[1]]][[items[2]]][[itemname]], "state") <- action
    } else if (l == 3) {
      if (!is.null(mnu[[items[1]]][[items[2]]][[items[3]]][[itemname]]))
        attr(mnu[[items[1]]][[items[2]]][[items[3]]][[itemname]],
          "state") <- action
    } else if (l == 4) {
      if (!is.null(
        mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[itemname]]))
        attr(mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[itemname]],
          "state") <- action
    } else if (l == 5) {
      if (!is.null(mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[
        items[5]]][[itemname]]))
        attr(mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[
          items[5]]][[itemname]], "state") <- action
    } else if (l > 5) {
      stop("You cannot use more than 5 menu levels")
    }
    # Save these changes
    .unix_menu_save(mnu)
    invisible(NULL)
  } else {
    .unix_menu_add(menuname, itemname, action)
  }
}

.unix_menu_del <- function(menuname) {
  mnu <- .Rmenu()
  items <- strsplit(as.character(menuname), "/", fixed = TRUE)[[1]]
  # Allow for a maximum of 5 sublevels (should be largely enough!)
  l <- length(items)
  if (l == 1 && !is.null(mnu[[items[1]]])) {
    mnu[[items[1]]] <- NULL
  } else if (l == 2 && !is.null(mnu[[items[1]]][[items[2]]])) {
    mnu[[items[1]]][[items[2]]] <- NULL
  } else if (l == 3 && !is.null(mnu[[items[1]]][[items[2]]][[items[3]]])) {
    mnu[[items[1]]][[items[2]]][[items[3]]] <- NULL
  } else if (l == 4 && !is.null(
    mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]])) {
    mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]] <- NULL
  } else if (l == 5 && !is.null(
    mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[items[5]]])) {
    mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[items[5]]] <- NULL
  } else return(invisible(NULL))
  # Save these changes
  .unix_menu_save(mnu)
  invisible(NULL)
}

.unix_menu_del_item <- function(menuname, itemname) {
  mnu <- .Rmenu()
  items <- strsplit(as.character(menuname), "/", fixed = TRUE)[[1]]
  # Allow for a maximum of 5 sublevels (should be largely enough!)
  l <- length(items)
  if (l == 1 && !is.null(mnu[[items[1]]][[itemname]])) {
    mnu[[items[1]]][[itemname]] <- NULL
  } else if (l == 2 && !is.null(mnu[[items[1]]][[items[2]]][[itemname]])) {
    mnu[[items[1]]][[items[2]]][[itemname]] <- NULL
  } else if (l == 3 && !is.null(
    mnu[[items[1]]][[items[2]]][[items[3]]][[itemname]])) {
    mnu[[items[1]]][[items[2]]][[items[3]]][[itemname]] <- NULL
  } else if (l == 4 && !is.null(
    mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[itemname]])) {
    mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[itemname]] <- NULL
  } else if (l == 5 && !is.null(
    mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[
      items[5]]][[itemname]])) {
    mnu[[items[1]]][[items[2]]][[items[3]]][[items[4]]][[
      items[5]]][[itemname]] <- NULL
  } else return(invisible(NULL))
  # Save these changes
  .unix_menu_save(mnu)
  invisible(NULL)
}
