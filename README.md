# sqlr.sqlite

<!-- badges: start -->
<!-- badges: end -->

The SQLite dialect for [sqlr](https://github.com/nbenn/sqlr). Renders a
sqlr schema as SQLite data definition language, and reflects a live
database back into that representation.

``` r
library(sqlr)
library(sqlr.sqlite)

schema <- sqlr_schema(
  "app",
  sqlr_table(
    "users",
    sqlr_column("id", sqlr_bigint(), null = FALSE),
    sqlr_column("email", sqlr_varchar(255), null = FALSE),
    sqlr_primary_key("id"),
    sqlr_unique("email")
  )
)

sqlr_render(schema, sqlite())
```

Given a connection, `sqlr_reflect()` reads the schema back out, and
`sqlr_equal()` compares the two.

## Installation

``` r
pak::pkg_install("nbenn/sqlr.sqlite")
```
