method(sqlr_defers_constraints, sqlite) <- function(dialect, ...) FALSE

method(sqlr_render_type, list(sqlr_integer_type, sqlite)) <-
  function(type, dialect, ...) {
    switch(
      as.character(type@bytes),
      "1" = "TINYINT",
      "2" = "SMALLINT",
      "4" = "INTEGER",
      "8" = "BIGINT"
    )
  }

method(sqlr_render_type, list(sqlr_float_type, sqlite)) <-
  function(type, dialect, ...) if (type@bytes == 4L) "REAL" else "DOUBLE"

method(sqlr_render_type, list(sqlr_decimal_type, sqlite)) <-
  function(type, dialect, ...) {
    if (is.na(type@precision)) {
      "NUMERIC"
    } else if (is.na(type@scale)) {
      paste0("NUMERIC(", type@precision, ")")
    } else {
      paste0("NUMERIC(", type@precision, ", ", type@scale, ")")
    }
  }

method(sqlr_render_type, list(sqlr_string_type, sqlite)) <-
  function(type, dialect, ...) {
    if (is.na(type@size)) {
      "TEXT"
    } else if (type@fixed) {
      paste0("CHAR(", type@size, ")")
    } else {
      paste0("VARCHAR(", type@size, ")")
    }
  }

method(sqlr_render_type, list(sqlr_binary_type, sqlite)) <-
  function(type, dialect, ...) "BLOB"

method(sqlr_render_type, list(sqlr_boolean_type, sqlite)) <-
  function(type, dialect, ...) "BOOLEAN"

method(sqlr_render_type, list(sqlr_time_type, sqlite)) <-
  function(type, dialect, ...) {
    switch(type@kind, date = "DATE", time = "TIME", "TIMESTAMP")
  }

method(sqlr_render_type, list(sqlr_json_type, sqlite)) <-
  function(type, dialect, ...) if (type@binary) "JSONB" else "JSON"

method(sqlr_render_type, list(sqlr_uuid_type, sqlite)) <-
  function(type, dialect, ...) "UUID"

method(sqlr_render_type, list(sqlr_other_type, sqlite)) <-
  function(type, dialect, ...) type@name

method(sqlr_render, list(sqlr_column, sqlite)) <-
  function(x, dialect, ...) {
    if (!is.null(x@identity)) {
      stop(
        "sqlite has no identity columns; column \"", x@name, "\" asks for one",
        call. = FALSE
      )
    }

    sqlr_render(x, super(dialect, sqlr_dialect), ...)
  }

method(sqlr_render, list(sqlr_foreign_key, sqlite)) <-
  function(x, dialect, ...) {
    x@ref_schema <- NA_character_
    sqlr_render(x, super(dialect, sqlr_dialect), qualifier = NA_character_)
  }

method(sqlr_render, list(sqlr_index, sqlite)) <-
  function(x, dialect, ..., table, qualifier = NA_character_) {
    if (is.na(x@name)) {
      stop("sqlite requires an index name", call. = FALSE)
    }

    keys <- paste0(
      sqlr_quote(dialect, x@columns),
      ifelse(x@desc, " DESC", ""),
      collapse = ", "
    )

    name <- if (is.na(qualifier)) {
      sqlr_quote(dialect, x@name)
    } else {
      paste0(sqlr_quote(dialect, qualifier), ".", sqlr_quote(dialect, x@name))
    }

    paste0(
      "CREATE ", if (x@unique) "UNIQUE " else "", "INDEX ", name,
      " ON ", sqlr_quote(dialect, table), " (", keys, ")"
    )
  }
