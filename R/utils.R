utctime = \() lubridate::now("UTC")
utcdate = \() lubridate::date(utctime())

is_scalar = \(x) is.atomic(x) && length(x) == 1L
is_non_null_scalar = \(x) if (!is.list(x) || !is.vector(x)) FALSE else !is.null(x) && is_scalar(x)
is_non_null_string = \(x) is_non_null_scalar(x) && is.character(x)
pick = \(x, keys) subset(x, names(x) %in% keys)
add_class = \(x, cls) structure(x, class = cls)
env_as = \(cls, include_private = FALSE) as.list(parent.frame(), all.names = include_private) |> add_class(cls)

assert_class = function(x, cls, msg = NULL) {
  if (is(x, cls)) return(invisible(NULL))
  if (is.null(msg)) msg = glue::glue("class of x is not {cls}")
  stop(msg)
}

assert_class_or_null = function(x, cls, msg = NULL) {
  if (is(x, "NULL") || is(x, cls)) return(invisible(NULL))
  if (is.null(msg)) msg = glue::glue("class of x is neither {cls} nor NULL")
  stop(msg)
}

bcrypt_hash = \(x) bcrypt::hashpw(x, bcrypt::gensalt(.BCRYPT_SALT_ROUNDS))

where = function(x, f, ...) {
  if (is.character(f)) return(Filter(\(xx) xx[[f]], x))
  ff = function(xx) f(xx, ...)
  Filter(ff, x)
}

where_not = function(x, f, ...) {
  if (is.character(f)) return(Filter(\(xx) !xx[[f]], x))
  ff = function(xx) !f(xx, ...)
  Filter(ff, x)
}

map = function(x, f, ...) {
  if (is.character(f)) return(Map(\(xx) xx[[f]], x))
  ff = function(xx) f(xx, ...)
  Map(ff, x)
}
