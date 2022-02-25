all_columns = \(entity) where(entity, is, .COLUMN_DECL)
db_columns = \(entity) all_columns(entity) |> where_not("transient")

sql_column_decl = function(column) {
  type = column$type
  pk = if (column$pk) " PRIMARY KEY" else ""
  notnull = if (is_required(column)) " NOT NULL" else ""
  isunique = if (column$unique) " UNIQUE" else ""
  glue::glue("{type}{pk}{notnull}{isunique}")
}

create_table = function(conn, entity) {
  columns = db_columns(entity)
  fields = columns |> map(sql_column_decl) |> setNames(names(columns))
  DBI::dbCreateTable(conn, entity$.table, fields)
}

insert_into = function(conn, entity, x) {
  keys = paste(names(x), collapse = ", ")
  values = names(x) |>
    map(\(f) entity[[f]]$db_parser(paste0("?", f))) |>
    paste(collapse = ", ")
  query = glue::glue("INSERT INTO {entity$.table}({keys}) VALUES ({values});")
  sql = DBI::sqlInterpolate(conn, query, .dots = x)
  DBI::dbExecute(conn, sql)
}

from_input = function(entity, x) {
  output = list()
  k = NULL
  for (field in all_columns(entity)) {
    if (field$transient) next

    k = field$name

    if (is_not_null(x[[k]]) || field$deduced) {
      output[[k]] = field$parser(x)
      next
    }

    if (has_default(field)) {
      output[[k]] = field$default()
      next
    }

    if (field$nullable) next

    stop(glue::glue("missing value declaration for field '{k}'"))
  }
  output
}
