#' @import S7
#' @import sqlr
#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
  sqlr::sqlr_register_dialect("SQLiteConnection", function(con) sqlite())
}
