Field = function(
  name = NULL,
  type = NULL,
  pk = FALSE,
  fk = NULL,
  default = NULL,
  nullable = FALSE,
  unique = FALSE,
  transient = FALSE,
  updatable = TRUE,
  update_trigger = NULL,
  parser = NULL,
  db_parser = identity,
  deduced = FALSE,
  .only_val = FALSE,
  .n = 1L
) {
  f = env_as(.FIELD_DECL) |> .validate_field()
  if (.only_val) return(f)

  entity = parent.frame(.n)
  entity[[f$name]] = f
}

ForeignKey = function(entity, field) {
  assert_class(entity, .ENTITY_DECL)
  assert_class(field, .FIELD_DECL)
  env_as(.FK_DECL)
}

StringField = function(name, parser = \(x) as.character(x[[name]])[1], .n = 2L, ...) {
  Field(name, "VARCHAR", parser = parser, .n = .n, ...)
}

StringArrayField = function(name, parser = \(x) as.character(x[[name]]), .n = 2L, ...) {
  Field(name, "VARCHAR[]", parser = parser, .n = .n, ...)
}

DoubleField = function(name, parser = \(x) as.numeric(x[[name]])[1], .n = 2L, ...) {
  Field(name, "DOUBLE PRECISION", parser = parser, .n = .n, ...)
}

DoubleArrayField = function(name, parser = \(x) as.numeric(x[[name]]), .n = 2L, ...) {
  Field(name, "DOUBLE PRECISION[]", parser = parser, .n = .n, ...)
}

IntegerField = function(name, parser = \(x) as.integer(x[[name]])[1], .n = 2L, ...) {
  Field(name, "INTEGER", parser = parser, .n = .n, ...)
}

IntegerArrayField = function(name, parser = \(x) as.integer(x[[name]]), .n = 2L, ...) {
  Field(name, "INTEGER[]", parser = parser, .n = .n, ...)
}

BigIntField = function(name, parser = \(x) bit64::as.integer64(x[[name]])[1], .n = 2L, ...) {
  Field(name, "BIGINT", parser = parser, .n = .n, ...)
}

BigIntArrayField = function(name, parser = \(x) bit64::as.integer64(x[[name]]), .n = 2L, ...) {
  Field(name, "BIGINT[]", parser = parser, .n = .n, ...)
}

DateField = function(name, parser = \(x) .date_parser(as.character(x[[name]]))[1], .n = 2L, ...) {
  Field(name, "DATE", parser = parser, .n = .n, ...)
}

DateArrayField = function(name, parser = \(x) .date_parser(as.character(x[[name]])), .n = 2L, ...) {
  Field(name, "DATE[]", parser = parser, .n = .n, ...)
}

TimestampField = function(name, parser = \(x) .timestamp_parser(as.character(x[[name]]))[1], .n = 2L, ...) {
  Field(name, "TIMESTAMP", parser = parser, .n = .n, ...)
}

TimestampArrayField = function(name, parser = \(x) .timestamp_parser(as.character(x[[name]])), .n = 2L, ...) {
  Field(name, "TIMESTAMP[]", parser = parser, .n = .n, ...)
}

UuidField = function(name, parser = \(x) as.character(x[[name]])[1], .n = 2L, ...) {
  Field(name, "UUID", parser = parser, .n = .n, ...)
}

UuidPrimaryKey = function(name, ...) {
  UuidField(name, pk = TRUE, default = uuid::UUIDgenerate, .n = 3L, ...)
}

timestamps = function() {
  TimestampField("created_at", default = utctime, updatable = FALSE, .n = 3L)
  TimestampField("updated_at", default = utctime, update_trigger = utctime, .n = 3L)
}

# private

.FIELD_DECL = "fielddecl"
.FK_DECL = "foreignkeydecl"

.has_default_val  = \(f) is_non_null_scalar(f$default)
.has_default      = \(f) is.function(f$default) || .has_default_val(f)
.is_fk            = \(f) is(f$fk, .FK_DECL)
.is_valid_fk      = \(f) is.null(f$fk) || .is_fk(f)
.cant_be_null     = \(f) f$pk || .is_fk(f) || f$unique || .has_default(f)

.date_parser      = \(x) lubridate::fast_strptime(x, "%Y-%m-%d", "UTC")
.timestamp_parser = \(x) lubridate::fast_strptime(x, "%Y-%m-%d %H:%M:%OS", "UTC")
.is_multi_array   = \(t) is_non_null_string(t) && endsWith(t, "[][]")

.validate_field = function(f) {
  if (is.null(f$name) || is.null(f$type)) stop("Field must have `name` and `type`")
  if (.is_multi_array(f$type)) {
    if (is.null(f$parser) || is.null(f$db_parser))
      stop("Nested array fields must define both `parser` and `db_parser`")
  }
  if (!.is_valid_fk(f)) stop("Non-null `fk`s should be a ForeingKey() call")
  if (.cant_be_null(f)) f$nullable = FALSE
  if (is.function(f$update_trigger)) f$updatable = TRUE
  if (!.has_default(f)) f$default = NULL else if (.has_default_val(f)) f$default = \() f$default
  f
}
