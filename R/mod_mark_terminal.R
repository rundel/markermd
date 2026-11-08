# The optional Terminal tab of mark(): a shinyterm tabset whose "+" menu
# starts a shell or an agent CLI (claude, codex) in the project root. shinyterm
# is a Suggests dependency; everything here is only reached when the user
# opts in through mark(terminal = ...).

# Resolve mark()'s `terminal` argument to the commands the tabset offers, or
# NULL when the tab is off. Checks that shinyterm is installed when it is on.
#
# terminal: FALSE (no tab), TRUE (a shell plus claude and codex when they are
#   on the PATH, all started in the project root) or a named list of commands
#   as accepted by shinyterm::terminal_tabs_server()
# root: the project root, the agents' working directory

resolve_mark_terminal = function(terminal, root) {
  if (isFALSE(terminal) || is.null(terminal)) {
    return(NULL)
  }
  rlang::check_installed("shinyterm", version = "0.0.0.9001", reason = "to show the Terminal tab in `mark()`.")

  if (isTRUE(terminal)) {
    terminal = shinyterm::default_commands(wd = root)
  }
  if (!is.list(terminal) && !is.character(terminal)) {
    cli::cli_abort(c(
      "{.arg terminal} must be {.code TRUE}, {.code FALSE}, or a named list of commands.",
      "i" = "See {.fn shinyterm::terminal_tabs_server} for the accepted command forms."
    ))
  }
  if (is.character(terminal)) {
    terminal = as.list(terminal)
  }
  # A bare program (the default shell among them) carries no working
  # directory, so it would start wherever R was launched
  lapply(terminal, function(cmd) {
    if (is.character(cmd)) shinyterm::terminal_command(cmd, wd = root) else cmd
  })
}

# The environment every terminal process starts with, so a shell or agent
# launched from the tab knows which project the running app grades.
#
# root: the project root

mark_terminal_env = function(root) {
  c(MARKERMD_PROJECT = normalizePath(root, mustWork = FALSE))
}

# The Terminal tab's content: the shinyterm tabset, filling the panel. It
# starts empty; the "+" menu opens a shell, claude or codex.
#
# id: module id

mark_terminal_ui = function(id) {
  # The tabset is a fill item, so it must sit directly in the fillable nav
  # panel (a plain wrapper div would size it to its content)
  shinyterm::terminal_tabs_ui(shiny::NS(id, "terms"))
}

# Server for the Terminal tab. Every terminal survives a page reload for
# `persist` seconds, so an agent session is not lost to an accidental refresh.
#
# id: module id
# commands: named list of commands from resolve_mark_terminal()
# root: the project root
# token: access token the page URL must carry, or NULL for none
# persist: seconds a process outlives its session, for a reload to reattach

mark_terminal_server = function(id, commands, root, token = NULL, persist = 300) {
  shiny::moduleServer(id, function(input, output, session) {
    shinyterm::terminal_tabs_server(
      "terms",
      commands = commands,
      initial = NULL,
      env = mark_terminal_env(root),
      token = token,
      persist = persist
    )
  })
}
