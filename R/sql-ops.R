#' Create a table in the database for the given `relation`.
#'
#' @param relation An `koral_relation` object.
#' @param drop_if_exists Logical. If `TRUE`, drops the table with the same name (in case
#'                       it exists) before creating the given one.
#'
#' @return Logical indicating whether the table was created.
#'
#' @rdname migrations
#' @export
create_table = function(relation, drop_if_exists = FALSE) {
  assert_class(.RELATION_DECL, relation)
  conn = .get_conn(); on.exit(DBI::dbDisconnect(conn))
  table = attr(relation, "table")

  if (DBI::dbExistsTable(conn, table)) {
    if (!drop_if_exists) {
      cat("Table already exists.\n")
      return(invisible(NULL))
    }
    DBI::dbRemoveTable(conn, table)
  }

  DBI::dbExecute(conn, .create_table_sql(relation))
  TRUE
}

#' Drops the table `attr(relation, "table")` in the database.
#'
#' @param relation An `koral_relation` object.
#'
#' @return Logical indicating whether the table was dropped.
#'
#' @rdname migrations
#' @export
drop_table = function(relation) {
  assert_class(.RELATION_DECL, relation)
  conn = .get_conn(); on.exit(DBI::dbDisconnect(conn))
  table = attr(relation, "table")

  if (!DBI::dbExistsTable(conn, table)) {
    cat("Table does not exist.\n")
    return(FALSE)
  }

  DBI::dbRemoveTable(conn, table)
  TRUE
}

#' Create tables for each `koral_relation` defined in the current scope.
#'
#' @inheritParams create_table
#'
#' @rdname migrations
#' @export
create_tables = function(drop_if_exists = FALSE) {
  for (relation in .all_relations(2)) create_table(relation, drop_if_exists)
}

#' Show the generated SQL for table creation of all `koral_relation` objects in
#' the current scope.
#'
#' @rdname migrations
#' @export
show_tables_sql = function() {
  for (relation in .all_relations(2)) cat(.create_table_sql(relation), "\n")
}

#' Insert an object into a table.
#'
#' @param relation An `koral_relation` object.
#' @param x A `list` representing the record to be parsed and saved.
#'
#' @return A `list` representing the saved record, including its generated fields.
#'
#' @export
insert = function(relation, x) {
  assert_class(.RELATION_DECL, relation)
  conn = .get_conn(); on.exit(DBI::dbDisconnect(conn))

  x = .from_input(relation, x)
  fields = names(x)
  query = sprintf(
    "INSERT INTO %s (%s) VALUES (%s) RETURNING *;",
    attr(relation, "table"),
    fields |> mkstring(", "),
    map(fields, \(f) relation[[f]]$db_parser(.sql_interpolator(f))) |> mkstring(", ")
  )
  .get_records(conn, DBI::sqlInterpolate(conn, query, .dots = x))[[1]]
}

#' Get all records from a given relation.
#'
#' @param relation An `koral_relation` object.
#' @param ... Key-value pairs representing filters applied to the table.
#' @param .limit Integer. If not `NULL`, includes a `LIMIT` clause.
#'
#' @return List of lists: inner lists contain the retrieved records.
#'
#' @export
get_all = function(relation, ..., .limit = NULL) {
  conn = .get_conn(); on.exit(DBI::dbDisconnect(conn))
  select_star = paste("SELECT * FROM", attr(relation, "table"))
  wheres = list(...)
  if (length(wheres) == 0) return(.get_records(conn, select_star))

  limit = if (is_positive_number(.limit)) paste("LIMIT", .limit) else ""
  query = paste(select_star, "WHERE", .sql_where(wheres, conn), limit)
  .get_records(conn, query |> rm_double_spaces() |> paste0(";"))
}

#' Get a single record satisfying
#'
#' @inheritParams
#' @inheritDotParams
#'
#' @return A `list` representing the record.
get_one = function(relation, ...) {
  maybe_record = get_all(relation, ..., .limit = 1)
  if (length(maybe_record) == 0) return(NULL)
  maybe_record[[1]]
}

# private

# TODO: parse/insert in C++
.df_to_list = function(df) df |> split(1:nrow(df)) |> map(as.list) |> unname()
.list_to_df = function(records_list) do.call(rbind, map(records_list, as.data.frame))
.sql_interpolator = function(name) paste0("?", name)
.db_fields = function(relation) relation |> where_not("transient")

.sql_where = function(wheres, .conn) {
  fields = names(wheres)
  sql = paste(fields, "=", .sql_interpolator(fields))
  DBI::sqlInterpolate(.conn, sql, .dots = wheres)
}

.get_records = function(conn, query) {
  df = DBI::dbGetQuery(conn, query)
  if (nrow(df) == 0) return(NULL)
  .df_to_list(df)
}

.as_sql_list = function(xs) {
  assert_class("character", xs)
  DBI::dbQuoteLiteral(DBI::ANSI(), xs) |>
    as.character() |>
    mkstring("(", ", ", ")")
}

.get_conn = function() {
  dbargs = getOption(.KORAL_DBARGS)
  if (is.null(dbargs)) stop(paste("R option", .KORAL_DBARGS, "is not set"))
  do.call(DBI::dbConnect, dbargs)
}

# TODO: parse/insert in C++
.from_input = function(relation, x) {
  .stop = \(k) stop(paste("missing value for field", k))

  output = list()
  for (field in relation) {
    k = field$name; v = x[[k]]

    if (field$transient) {
      if (is.null(v) && !field$nullable) .stop(k)
      next
    }

    if (field$deduced || !is.null(v)) {
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
