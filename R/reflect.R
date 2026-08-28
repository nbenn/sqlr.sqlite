method(sqlr_reflect_schema, sqlite) <-
  function(dialect, con, schema = NULL, ...) {
    names <- DBI::dbGetQuery(
      con,
      "SELECT name FROM sqlite_master
        WHERE type = 'table' AND name NOT LIKE 'sqlite_%'
        ORDER BY name"
    )$name

    tables <- lapply(names, function(nm) reflect_table(con, nm))

    do.call(
      sqlr_schema,
      c(list(name = if (is.null(schema)) "main" else schema), tables)
    )
  }

reflect_table <- function(con, name) {
  info <- pragma(con, "table_info", name)

  parts <- c(
    lapply(seq_len(nrow(info)), function(i) reflect_column(info[i, ])),
    reflect_primary_key(info),
    reflect_foreign_keys(con, name),
    reflect_indexes(con, name)
  )

  do.call(sqlr_table, c(list(name = name), parts))
}

reflect_column <- function(row) {
  sqlr_column(
    name = row$name,
    type = as_sqlr_type(tolower(row$type)),
    null = row$notnull == 0L,
    default = parse_default(row$dflt_value)
  )
}

reflect_primary_key <- function(info) {
  keys <- info[info$pk > 0L, , drop = FALSE]
  if (!nrow(keys)) {
    return(list())
  }

  list(sqlr_primary_key(keys$name[order(keys$pk)]))
}

reflect_foreign_keys <- function(con, name) {
  rows <- pragma(con, "foreign_key_list", name)
  if (!nrow(rows)) {
    return(list())
  }

  lapply(unique(rows$id), function(id) {
    fk <- rows[rows$id == id, , drop = FALSE]
    fk <- fk[order(fk$seq), , drop = FALSE]

    targets <- fk$to
    if (anyNA(targets)) {
      targets <- referenced_key(con, fk$table[[1L]])
    }

    sqlr_foreign_key(
      columns = fk$from,
      ref_table = fk$table[[1L]],
      ref_columns = targets,
      on_delete = tolower(fk$on_delete[[1L]]),
      on_update = tolower(fk$on_update[[1L]])
    )
  })
}

referenced_key <- function(con, table) {
  info <- pragma(con, "table_info", table)
  keys <- info[info$pk > 0L, , drop = FALSE]

  keys$name[order(keys$pk)]
}

reflect_indexes <- function(con, name) {
  rows <- pragma(con, "index_list", name)
  if (!nrow(rows)) {
    return(list())
  }

  out <- lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]
    if (identical(row$origin, "pk")) {
      return(NULL)
    }

    columns <- pragma(con, "index_info", row$name)
    columns <- columns$name[order(columns$seqno)]

    if (identical(row$origin, "u")) {
      sqlr_unique(columns)
    } else {
      sqlr_index(columns, name = row$name, unique = row$unique == 1L)
    }
  })

  out[!vapply(out, is.null, logical(1L))]
}

pragma <- function(con, name, arg) {
  DBI::dbGetQuery(
    con,
    paste0("PRAGMA ", name, "(", DBI::dbQuoteIdentifier(con, arg), ")")
  )
}

parse_default <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    return(NULL)
  }

  if (grepl("^'.*'$", x)) {
    return(gsub("''", "'", substr(x, 2L, nchar(x) - 1L), fixed = TRUE))
  }

  if (toupper(x) %in% c("TRUE", "FALSE")) {
    return(toupper(x) == "TRUE")
  }

  if (grepl("^-?[0-9]+$", x)) {
    return(as.integer(x))
  }

  if (grepl("^-?[0-9]*\\.[0-9]+$", x)) {
    return(as.numeric(x))
  }

  sqlr_sql(text = x)
}
