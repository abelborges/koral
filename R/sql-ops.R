create_table = function(entity, drop_if_exists = FALSE) {
  assert_class(entity, .ENTITY_DECL)
  conn = attr(entity, "get_conn")(); on.exit(DBI::dbDisconnect(conn))
  table = attr(entity, "table")

  if (DBI::dbExistsTable(conn, table)) {
    if (!drop_if_exists) {
      cat("Table already exists.\n")
      return(invisible(NULL))
    }
    DBI::dbRemoveTable(conn, table)
  }

  fields = .db_fields(entity)
  columns = fields |> map(.sql_field_decl) |> setNames(names(fields))
  DBI::dbCreateTable(conn, table, columns)
}

drop_table = function(entity) {
  assert_class(entity, .ENTITY_DECL)
  conn = attr(entity, "get_conn")(); on.exit(DBI::dbDisconnect(conn))
  table = attr(entity, "table")

  if (!DBI::dbExistsTable(conn, table)) {
    cat("Table does not exist.\n")
    return(invisible(NULL))
  }

  DBI::dbRemoveTable(conn, table)
}

insert = function(entity, x) {
  assert_class(entity, .ENTITY_DECL)
  conn = attr(entity, "get_conn")(); on.exit(DBI::dbDisconnect(conn))
  table = attr(entity, "table")

  x = .from_input(entity, x)
  keys = paste(names(x), collapse = ", ")
  values = names(x) |>
    map(\(f) entity[[f]]$db_parser(paste0("?", f))) |>
    paste(collapse = ", ")
  stmt = glue::glue("INSERT INTO {table}({keys}) VALUES ({values}) RETURNING *;")
  DBI::dbGetQuery(conn, DBI::sqlInterpolate(conn, stmt, .dots = x))
}

# private

.db_fields = \(entity) entity |> where_not("transient")

# TODO: parse/insert in C++
.from_input = function(entity, x) {
  .stop = \(k) stop(glue::glue("missing value declaration for field '{k}'"))
  output = list()
  for (field in entity) {
    k = field$name; v = x[[k]]
    if (!field$nullable && is.null(v)) .stop(k)
    if (field$transient) next

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

.sql_field_decl = function(field) {
  type = field$type
  pk = if (field$pk) " PRIMARY KEY" else ""
  notnull = if (!field$nullable) " NOT NULL" else ""
  isunique = if (field$unique) " UNIQUE" else ""
  glue::glue("{type}{pk}{notnull}{isunique}")
}
