create_table = function(entity, drop_if_exists) UseMethod("create_table")
create_table.entitydecl = function(entity, drop_if_exists = FALSE) {
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

drop_table = function(entity) UseMethod("drop_table")
drop_table.entitydecl = function(entity) {
  conn = attr(entity, "get_conn")(); on.exit(DBI::dbDisconnect(conn))
  table = attr(entity, "table")

  if (!DBI::dbExistsTable(conn, table)) {
    cat("Table does not exist.\n")
    return(invisible(NULL))
  }

  DBI::dbRemoveTable(conn, table)
}

insert = function(entity, x) UseMethod("insert")
insert.entitydecl = function(entity, x) {
  conn = attr(entity, "get_conn")(); on.exit(DBI::dbDisconnect(conn))
  table = attr(entity, "table")

  x = .from_input(entity, x)
  keys = paste(names(x), collapse = ", ")
  values = names(x) |>
    map(\(f) entity[[f]]$db_parser(paste0("?", f))) |>
    paste(collapse = ", ")
  stmt = glue::glue("INSERT INTO {table}({keys}) VALUES ({values});")
  DBI::dbExecute(conn, DBI::sqlInterpolate(conn, stmt, .dots = x))
}

# private

.db_fields = \(entity) entity |> where_not("transient")

# TODO: parse/insert in C++
.from_input = function(entity, x) {
  output = list()
  for (field in entity) {
    if (field$transient) next

    k = field$name

    if (field$deduced || !is.null(x[[k]])) {
      output[[k]] = field$parser(x)
      next
    }

    if (.has_default(field)) {
      output[[k]] = field$default()
      next
    }

    if (field$nullable) next

    stop(glue::glue("missing value declaration for field '{k}'"))
  }
  output
}

.sql_field_decl = function(field) {
  type = field$type
  pk = if (field$pk) " PRIMARY KEY" else ""
  notnull = if (.is_required(field)) " NOT NULL" else ""
  isunique = if (field$unique) " UNIQUE" else ""
  glue::glue("{type}{pk}{notnull}{isunique}")
}
