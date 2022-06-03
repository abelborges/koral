#' Base field declaration constructor
#'
#' Use `Field` directly only for non-trivial cases.
#' Otherwise, checkout higher-level constructors such as
#' `UuidPrimaryKey`, `StringField`, `EnumField`, `timestamps` etc.
#'
#' @param name Field name.
#' @param type Field PostgreSQL type.
#' @param many If `TRUE`, field is modeled as an array of values of type `type`.
#'             Defaults to `FALSE`.
#' @param pk Logical. Is it part of the relation's Primary Key?
#' @param fk Set to a `ForeignKey`-returned object in case the field is constrained to
#'           the values of a field in another relation.
#' @param default Fill with either a function or a value to be used in `INSERT` queries
#'                in case input data skips this field.
#' @param nullable Logical. Is it allowed to be null?
#' @param unique Logical. Should there be a 1:1 mapping between the field and the PK?
#' @param transient Logical. Is it saved to the database? E.g., you probably want to
#'                  declare raw password fields with `transient = TRUE`, so that they
#'                  are encrypted before being persisted.
#' @param updatable Logical. Is it allowed to be updated after record creation?
#' @param update_trigger Fill with a 0-parameter function whose return value is used
#'                       to update the field during `UPDATE` queries.
#' @param parser Function of a named list representing a record whose return value is
#'               the parsed field value. This is called whenever `{koral}` is about to
#'               commit a write (insert or update) to the database.
#' @param db_parser Function of the field's name used to wrap the field's value in a
#'                  database function during writes.
#' @param db_check Optional string with PostgreSQL `CHECK` constraint expression. E.g.
#'                 `Field("positive", "NUMERIC", db_check = "CHECK (positive > 0)")`.
#' @param deduced Logical. Is it deduced from other fields (by `parser`)? For instance,
#'                `Field("hashed_x", "TEXT", deduced = TRUE, parser = \(dto) hash(dto$x))`.
#'
#' @return A `koral_field` object.
#' @rdname field
#' @export
Field = function(
  name = NULL,
  type = NULL,
  many = FALSE,
  pk = FALSE,
  fk = NULL,
  default = \() NULL,
  nullable = FALSE,
  unique = FALSE,
  transient = FALSE,
  updatable = TRUE,
  update_trigger = NULL,
  parser = NULL,
  db_parser = identity,
  db_check = NULL,
  deduced = FALSE
) env_as(.FIELD_DECL) |> .validate_field()

#' Reference a field from another table.
#'
#' @param relation An `koral_relation` object.
#' @param field The name of the referenced field. Required in case `relation` has a
#'              composite key. If `NULL`, defined as the PK at `relation`.
ForeignKey = function(relation, field = NULL) {
  assert_class(.RELATION_DECL, relation)
  if (is.null(field)) {
    if (.has_composite_pk(relation)) stop("Must specify field name for entities with composite PKs")
    field = relation |> where("pk") |> map("name") |> first_scalar()
  }
  assert_class("character", field)
  env_as(.FK_DECL)
}

#' @rdname field
#' @export
StringField = function(name, parser = \(x) as.character(x[[name]])[1], ...) {
  Field(name, "VARCHAR", parser = parser, ...)
}

#' @rdname field
#' @export
EnumField = function(name, values, parser = \(x) as.character(x[[name]])[1], ...) {
  assert_class("character", values)
  if (length(values) == 0L) stop("Empty enum `values`")
  if (length(values) == 1L) warning("Declaring enum field with a single value")
  db_check = sprintf("CHECK (%s IN %s)", name, .as_sql_list(values))
  Field(name, "VARCHAR", parser = parser, db_check = db_check, ...)
}

#' @rdname field
#' @export
DoubleField = function(name, parser = \(x) as.numeric(x[[name]])[1], ...) {
  Field(name, "DOUBLE PRECISION", parser = parser, ...)
}

#' @rdname field
#' @export
IntegerField = function(name, parser = \(x) as.integer(x[[name]])[1], ...) {
  Field(name, "INTEGER", parser = parser, ...)
}

#' @rdname field
#' @export
BigIntField = function(name, parser = \(x) bit64::as.integer64(x[[name]])[1], ...) {
  Field(name, "BIGINT", parser = parser, ...)
}

#' @rdname field
#' @export
DateField = function(name, parser = \(x) .date_parser(as.character(x[[name]]))[1], ...) {
  Field(name, "DATE", parser = parser, ...)
}

#' @rdname field
#' @export
TimestampField = function(name, parser = \(x) .timestamp_parser(as.character(x[[name]]))[1], ...) {
  Field(name, "TIMESTAMP", parser = parser, ...)
}

#' @rdname field
#' @export
UuidField = function(name, parser = \(x) as.character(x[[name]])[1], ...) {
  Field(name, "UUID", parser = parser, ...)
}

#' @rdname field
#' @export
UuidPrimaryKey = function(name, ...) {
  UuidField(name, pk = TRUE, default = uuid::UUIDgenerate, ...)
}

#' @rdname field
#' @export
timestamps = function() list(
  TimestampField("created_at", default = utctime, updatable = FALSE),
  TimestampField("updated_at", default = utctime, update_trigger = utctime)
)

# private

.has_default_val  = function(f) is.atomic(f$default) && !is.null(f$default)
.valid_default    = function(f) is.function(f$default) || .has_default_val(f)
.is_fk            = function(f) is(f$fk, .FK_DECL)
.is_valid_fk      = function(f) is.null(f$fk) || .is_fk(f)
.cant_be_null     = function(f) f$pk || .is_fk(f) || f$unique || .valid_default(f)

.date_parser      = function(x) lubridate::fast_strptime(x, "%Y-%m-%d", "UTC")
.timestamp_parser = function(x) lubridate::fast_strptime(x, "%Y-%m-%d %H:%M:%OS", "UTC")
.is_multi_array   = function(t) is_non_null_string(t) && endsWith(t, "[][]")

.validate_field = function(f) {
  if (is.null(f$name) || is.null(f$type)) stop("Field must have `name` and `type`")
  if (.is_multi_array(f$type)) {
    if (is.null(f$parser) || is.null(f$db_parser))
      stop("Nested array fields must define both `parser` and `db_parser`")
  }
  if (!.is_valid_fk(f)) stop("Non-null `fk`s should be a ForeingKey() call")
  if (.cant_be_null(f)) f$nullable = FALSE
  if (is.function(f$update_trigger)) f$updatable = TRUE
  if (!.valid_default(f)) f$default = \() NULL else if (.has_default_val(f)) f$default = \() f$default
  if (f$many) f$type = paste0(f$type, "[]")

  f
}

.field_decl_sql = function(field) {
  notnull = if (!field$nullable) "NOT NULL" else ""
  isunique = if (field$unique) "UNIQUE" else ""
  dbcheck = if (!is.null(field$db_check)) field$db_check else ""
  decl = paste(field$name, field$type, notnull, isunique, dbcheck)
  rm_double_spaces(decl)
}
