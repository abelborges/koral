utctime = \() round(1000 * as.numeric(.POSIXct(Sys.time(), "GMT"))) / 1000.0
is_null = \(x) is.null(x) || is.na(x)
is_not_null = \(x) !is_null(x)
pick = \(x, keys) subset(x, names(x) %in% keys)
add_class = \(x, cls) structure(x, class = cls)
bcrypt_hash = \(x) bcrypt::hashpw(x, bcrypt::gensalt(.BCRYPT_SALT_ROUNDS))

where = function(x, f, ...) {
  if (is.character(f)) return(Filter(\(xx) xx[[f]], x))
  ff = function(x) f(x, ...)
  Filter(ff, x)
}

where_not = function(x, f, ...) {
  if (is.character(f)) return(Filter(\(xx) !xx[[f]], x))
  ff = function(x) !f(x, ...)
  Filter(ff, x)
}

map = function(x, f, ...) {
  if (is.character(f)) return(Map(\(xx) xx[[f]], x))
  ff = function(x) f(x, ...)
  Map(ff, x)
}
