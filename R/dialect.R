#' The SQLite dialect
#'
#' Passed to [sqlr::sqlr_render()] to emit SQLite, and resolved automatically
#' from a `SQLiteConnection` by [sqlr::sqlr_for()].
#'
#' SQLite stores the declared type of a column verbatim and applies affinity
#' rules to it, so types render under their standard names and are read back by
#' the same parser that handles authored spellings.
#'
#' Two limitations are worth knowing. Check constraints cannot be reflected:
#' SQLite records them only inside the original `CREATE TABLE` text, and
#' recovering them would mean parsing DDL. Identity columns are not supported
#' either, because expressing one means folding the primary key into the column
#' definition, which the rendering protocol has no hook for yet.
#'
#' @param version SQLite library version.
#' @param attrs Named list of dialect-specific attributes.
#'
#' @return A `sqlite` dialect object.
#'
#' @examples
#' sqlr::sqlr_render(
#'   sqlr::sqlr_table("t", sqlr::sqlr_column("id", sqlr::sqlr_int())),
#'   sqlite()
#' )
#'
#' @export
sqlite <- new_class("sqlite", parent = sqlr_dialect)
