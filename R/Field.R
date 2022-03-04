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

StringField         = \(name, parser = \(x) as.character(x[[name]])[1], ...) .field(name, "VARCHAR", parser = parser, ...)
StringArrayField    = \(name, parser = \(x) as.character(x[[name]]), ...) .field(name, "VARCHAR[]", parser = parser, ...)
DoubleField         = \(name, parser = \(x) as.numeric(x[[name]])[1], ...) .field(name, "DOUBLE PRECISION", parser = parser, ...)
DoubleArrayField    = \(name, parser = \(x) as.numeric(x[[name]]), ...) .field(name, "DOUBLE PRECISION[]", parser = parser, ...)
IntegerField        = \(name, parser = \(x) as.integer(x[[name]])[1], ...) .field(name, "INTEGER", parser = parser, ...)
IntegerArrayField   = \(name, parser = \(x) as.integer(x[[name]]), ...) .field(name, "INTEGER[]", parser = parser, ...)
BigIntField         = \(name, parser = \(x) bit64::as.integer64(x[[name]])[1], ...) .field(name, "BIGINT", parser = parser, ...)
BigIntArrayField    = \(name, parser = \(x) bit64::as.integer64(x[[name]]), ...) .field(name, "BIGINT[]", parser = parser, ...)
DateField           = \(name, parser = \(x) .date_parser(as.character(x[[name]]))[1], ...) .field(name, "DATE", parser = parser, ...)
DateArrayField      = \(name, parser = \(x) .date_parser(as.character(x[[name]])), ...) .field(name, "DATE[]", parser = parser, ...)
TimestampField      = \(name, parser = \(x) .timestamp_parser(as.character(x[[name]]))[1], ...) .field(name, "TIMESTAMP", parser = parser, ...)
TimestampArrayField = \(name, parser = \(x) .timestamp_parser(as.character(x[[name]])), ...) .field(name, "TIMESTAMP[]", parser = parser, ...)
UuidField           = \(name, parser = \(x) as.character(x[[name]])[1], ...) .field(name, "UUID", parser = parser, ...)

UuidPrimaryKey = \(name, ...) UuidField(name, pk = TRUE, default = uuid::UUIDgenerate, .n = 4L, ...)

timestamps = function() {
  entity = parent.frame()
  entity$created_at = TimestampField("created_at", default = utctime, updatable = FALSE, .only_val = TRUE)
  entity$updated_at = TimestampField("updated_at", default = utctime, update_trigger = utctime, .only_val = TRUE)
}

# private

.FIELD_DECL = "fielddecl"
.FK_DECL = "foreignkeydecl"

.field = \(name, type, .n = 3L, ...) Field(name, type, .n = .n, ...)

.has_default_val  = \(f) is_non_null_scalar(f$default)
.has_default      = \(f) is.function(f$default) || .has_default_val(f)
.is_required      = \(f) f$pk || .has_default(f) || !f$nullable
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
