# svDialogs 1.0.3

-   The `filters=` argument should be correctly handled now in `dlg_open()` & `dlg_save()` (bug corrected).

# svDialogs 1.0.2

-   For unix menu items, we now use `xvkbd -text 'cmd\n'` instead of `xvkbd -xsendevent -text 'cmd\n'`, because the `-xsendevent` is ignored by many recent apps. Also, `\n` and `\t` are now supported inside menu commands.

# svDialogs 1.0.1

-   Argument `rstudio=` to allow by-pass RStudio dialog boxes in native version when RStudio Desktop is used (thanks to Paul Hibbins).

-   In the case of 'RStudio Desktop', a Tk version of the list selection dialog box is used by default.

-   For `dlg_open(multiple = TRUE)` an OS-native dialog box is used instead of the 'RStudio' version that is limited to one file only in 'RStudio Desktop'. For 'RStudio Server' we have not found a solution yet. So, a warning indicates that only one file can be selected there for the moment.

# svDialogs 1.0.0

-   Moved to a GitHub repository.

-   Snake_case versions of the function names (but camelCase name kept for backward compatibility.

-   Documentation moved to Roxygen2, tests to testhat (bot no tests yet) and a vignette with knitr are added.

-   A bug that imposed to load svDialogs on the search path for some dialog boxes to be displayed under Windows should be solved.

-   RStudio version for many dialog boxes: dir, open (but no multiple selections), save, input & message. List dialog box is handled correctly under RStudio, although a text version at the R console is used instead (no GUI dialog yet.)

-   In case both 'zenity' and 'yad' are available under Linux, 'yad' is used in priority for all dialog boxes (and it is the only alternative for dlg_form()).

# svDialogs 0.9-59

-   `menuAdd()` and similar functions now build a more robust code under Unix/Linux for using the `ctxmenu` program. It now uses `xvkbd -xsendevent -text` command instead of just `xvkbd -text` that made problem on non-US keymap sometimes.

-   Entries in the default menu are made more robust: `Load package...` and `Install packages...` instructions are simplified, and `R project home page` and `CRAN home page` now use `browseURL()` instead of relying on Firefox.

-   Added a `Fix()` function in the temporary environment that just duplicate `fix()` in case it does not already exists. The menu entry for data edition uses it.

# svDialogs 0.9-58

-   `dlgInput()` returned a strange result in Mac OS X El Capitan, due to different string formatting of the underlying Mac code.

# svDialogs 0.9-57

-   Added requirements for Linux (`zenity`) in the `DESCRIPTION` file.

# svDialogs 0.9-56

-   The native Mac dialogs did not work any more, solved for 10.5 Leopard and above by querying the name of the application by its id (Rgui).

# svDialogs 0.9-55

-   Rework of `Author` and `Authors@R` fields in the `DESCRIPTION` file.

# svDialogs 0.9-54

-   Dependencies to {tcltk} and {svMisc} are eliminated. Consequently, all functions that depend on Tcl/Tk are eliminated too. It concerns `guiDlg()`, `guiDlgFunction()` and other associated methods or functions. The new `dlgForm()`function should be used instead.

-   Temporary objects are now saved in `SciViews:TempEnv` instead of `TempEnv`.

# svDialogs 0.9-53

-   Added support for `JGR` in dialog boxes under Mac OS X.

-   The `menuXXX()` functions can manage menus and submenus in `JGR`.

# svDialogs 0.9-52

-   `menuXXX()` functions do not generate files in `\tmp` dir on Linux, unless in `interactive()` session, and with explicit user's acknowledgement. Moreover, the user name is not used anymore as part of the name of the temporary menu files generated (cf CRAN policies and following B. Ripley's request).

-   Code to manage `menuXXX()` items on Mac OS X is considered still experimental (on R-forge, not CRAN) and is now commented out. Will be further developed in a future version of the {svDialogs} package.

# svDialogs 0.9-51

-   `NEWS` file reworked to use the new Rd format.

# svDialogs 0.9-50

-   Slight changes in flexible dialog functions according to notes generated by R CMD check (2.15.0), i.e., partial matching of argument env(ir) and use of `.Internal` in `eval.with.vis()`; replaced by the actual function `withVisible()`.

# svDialogs 0.9-49

-   Added `dlgForm()` for flexible form dialog box. Only the Linux implementation using `yad` and the textual version are currently implemented.

# svDialogs 0.9-48

-   Argument message is changed to title in `dlgDir()` function, to match corresponding argument in `dlgOpen()` and `dlgSave()` and also to indicate it can only be a single line of text!

-   Added `msgBox()` and `okCancelBox()` function for simpler message box handling.

-   `dlgOpen()` is now implemented and its `textCLI` version also accepts single and double quotes around file path (allow to drag&drop from, e.g., nautilus to gnome-terminal in Gnome Linux), on the contrary to `file.choose()`.

-   `dlgFilters` is similar to Filters matrix under Windows, and it provides a series of default file types and filters for `dlgOpen()` and `dlgSave()`.

-   `dlgSave()` is also implemented, but it uses `choose.files()` on Windows, which is merely designed to open file(s) instead of providing a file name to save to.

# svDialogs 0.9-47

-   Now, `menuAddItem()` implements `'enable'` and `'disable'` in action to change the state of an existing menu item.

-   On Windows, using an action as `'enable'` or `'disable'` on nonexisting menus just does nothing (on the contrary to the original `winMenuAddItem()` which creates that menus with the action being `'enable'`d or `'disable'`d, respectively).

-   `'$ConsoleMain/<menu>'` is now accepted as a synonym of `'<menu>'` in all `menuXXX()` functions, according to corresponding `winMenuXXX()` functions.

-   The `menuXXX()` functions now check for bad syntax with the special menus `$ConsoleMain`, `$ConsolePopup`, `$Graph<n>Main` and `$Graph<n>Popup:` special menu not followed by at least one submenu.

-   The console context menu is now implemented too, and `'$ConsolePopup/<menu>'` menus allow for adding submenus to it.

-   Addition of `menuNames()` and `menuItems()`, working the same way as `winMenuNames()` and `winMenuItems()` under Windows.

# svDialogs 0.9-46

-   The functions to handle menus in Linux are completely rewritten to use a menu configuration file that a modified version of `myGtkMenu` (named `ctxmenu`) can read and interpret to display the corresponding menus.

# svDialogs 0.9-45

-   Similar custom menus as `winMenuXXX()` functions are added and allow to add custom menus on the Mac (both `R.app` and `terminal`) and for R run on a Gnome desktop, providing the system is configured to manage such menus, see `?menuAdd`.

# svDialogs 0.9-44

-   The `guiDlgXXX()` functions are reworked into S3 methods and their interface changes. To avoid any confusion, they are renamed `dlgXXX()`.

-   `dlgMessage()` is reworked into native dialog box, but it looses a couple of options during the process (title, icon, parent). The previous code is now moved to {svDialogstcltk}.

-   `dlgInput()` is reworked the same way, and it looses the `parent=` argument that was not implemented yet, anyway, and the `title=` argument that is now always `"question"` in order to match `winDialogString()` function on Windows.

-   `dlgList()` is also refactored that way. Its interface is completely changed to better match the arguments of `select.list()` and to make it a direct replacement for that function.

-   `dlgDir()` is completely rewritten, as well as, `dlgOpen()` and `dlgSave()`.

# svDialogs 0.9-43

-   {tcltk} R package is moved from `depends` to `imports`.

# svDialogs 0.9-42

-   `guiDlgFun()` is adapted to the new help system provided in R 2.10.

# svDialogs 0.9-41

-   When the path contained spaces, `guiDlgOpen()` and `guiDlgSave()` returned them in pieces.

# svDialogs 0.9-40

-   This is the first version distributed on R-forge. It is completely refactored from older versions (on CRAN since 2003) to make it run with **SciViews-K** and **Komodo Edit** (**SciViews-R Console** not supported any more).