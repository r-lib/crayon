
local_colors <- function(colors = 256, .local_envir = parent.frame()) {
  withr::local_envvar(
    list(R_CLI_NUM_COLORS = as.character(colors)),
    .local_envir = .local_envir
  )
}
