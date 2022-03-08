create_table = function(entity, drop_if_exists = FALSE) {
  assert_class(.ENTITY_DECL, entity)
  conn = .get_conn(); on.exit(DBI::dbDisconnect(conn))
  table = attr(entity, "table")

  if (DBI::dbExistsTable(conn, table)) {
    if (!drop_if_exists) {
      cat("Table already exists.\n")
      return(invisible(NULL))
    }
    DBI::dbRemoveTable(conn, table)
  }

  DBI::dbExecute(conn, .create_table_sql(entity))
}

drop_table = function(entity) {
  assert_class(.ENTITY_DECL, entity)
  conn = .get_conn(); on.exit(DBI::dbDisconnect(conn))
  table = attr(entity, "table")

  if (!DBI::dbExistsTable(conn, table)) {
    cat("Table does not exist.\n")
    return(invisible(NULL))
  }

  DBI::dbRemoveTable(conn, table)
}

insert = function(entity, x) {
  assert_class(.ENTITY_DECL, entity)
  conn = .get_conn(); on.exit(DBI::dbDisconnect(conn))
  table = attr(entity, "table")

  x = .from_input(entity, x)
  keys = x |> names() |> mkstring(", ")
  values = names(x) |>
    map(\(f) entity[[f]]$db_parser(prepend(f, "?"))) |>
    mkstring(", ")
  stmt = glue::glue("INSERT INTO {table}({keys}) VALUES ({values}) RETURNING *;")
  DBI::dbGetQuery(conn, DBI::sqlInterpolate(conn, stmt, .dots = x))
}

# private

.db_fields = \(entity) entity |> where_not("transient")

# TODO: parse/insert in C++
.from_input = function(entity, x) {
  .stop = \(k) stop(sprintf("missing value declaration for field '%s'", k))
  output = list()
  for (field in entity) {
    k = field$name
    v = x[[k]]

    if (field$transient) {
      if (!field$nullable && is.null(v)) .stop(k)
      next
    }

    if (field$deduced || !is.null(x[[k]])) {
      output[[k]] = field$parser(x)
      next
    }

    if (.has_default(field)) {
      output[[k]] = field$default()
      next
    }

    if (field$nullable) next

    .stop(k)
  }
  output
}

.as_sql_list = function(xs) {
  assert_class("character", xs)
  DBI::dbQuoteLiteral(DBI::ANSI(), xs) |>
    as.character() |>
    mkstring("(", ", ", ")")
}

.get_conn = function() {
  dbargs = getOption(.KORAL_DBARGS)
  if (is.null(dbargs)) stop(glue::glue("R option {.KORAL_DBARGS} should be set"))
  do.call(DBI::dbConnect, dbargs)
}
