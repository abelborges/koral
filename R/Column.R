.COLUMN_DECL = "columndecl"

Column = function(
  name = NULL,
  type = "VARCHAR",
  pk = FALSE,
  references = NULL,
  nullable = FALSE,
  default = NULL,
  unique = FALSE,
  transient = FALSE,
  updatable = TRUE,
  update_trigger = NULL,
  parser = \(x) as.character(x[[name]])[1],
  db_parser = identity,
  deduced = FALSE
) {
  if (is.null(name)) stop("Column name cannot be null")
  column = environment() |> as.list() |> add_class(.COLUMN_DECL)
  entity = parent.frame()
  entity[[column$name]] = column
  invisible(column)
}

UuidPrimaryKey = function(name = "id") {
  entity = parent.frame()
  column = Column(name, "UUID", pk = TRUE, default = uuid::UUIDgenerate)
  entity[[name]] = column
  invisible(column)
}

Timestamp = function(name, ...) {
  entity = parent.frame()
  column = Column(name, "TIMESTAMP", default = utctime, db_parser = \(x) glue::glue("to_timestamp({x})"), ...)
  entity[[name]] = column
  invisible(column)
}

timestamps = function() {
  entity = parent.frame()
  entity$created_at = Timestamp("created_at", updatable = FALSE)
  entity$updated_at = Timestamp("updated_at", update_trigger = utctime)
  invisible(NULL)
}

has_default = \(column) is.function(column$default)
is_required = \(column) column$pk || has_default(column) || !column$nullable
