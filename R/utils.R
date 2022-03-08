utctime = \() lubridate::now("UTC")
utcdate = \() lubridate::date(utctime())
println = \(...) cat(..., "\n")
printlns = \(...) cat(..., sep = "\n")

prepend = \(x, prefix) paste0(prefix, x)

mkstring = function(xs, ...) {
  seps = list(...)
  if (length(seps) == 0L) return(paste(xs, collapse = " "))
  if (length(seps) == 1L) return(paste(xs, collapse = seps[[1]]))
  if (length(seps) == 2L) {
    prefix = seps[[1]]; sep = seps[[2]]
    return(paste0(prefix, paste(xs, collapse = sep)))
  }
  if (length(seps) == 3L) {
    prefix = seps[[1]]; sep = seps[[2]]; postfix = seps[[3]]
    return(paste0(prefix, paste(xs, collapse = sep), postfix))
  }
  stop("Wrong number of arguments")
}

not = \(x) !x
not_null = \(x) !is.null(x)
is_scalar = \(x) is.atomic(x) && length(x) == 1L
is_non_null_scalar = \(x) if (!is.list(x) || !is.vector(x)) FALSE else !is.null(x) && is_scalar(x)
is_non_null_string = \(x) is_non_null_scalar(x) && is.character(x)
pick = \(x, keys) subset(x, names(x) %in% keys)
add_class = \(x, cls) structure(x, class = cls)
env_as = \(cls, include_private = FALSE) as.list(parent.frame(), all.names = include_private) |> add_class(cls)
as_vec = \(x) unlist(x, recursive = TRUE, use.names = FALSE)
first_scalar = \(x) as_vec(x)[[1]]

assert_class = function(cls, ...) {
  for (x in list(...)) {
    if (!is(x, cls)) stop(glue::glue("class of {x} is not {cls}"))
  }
  return(invisible(TRUE))
}

assert_class_or_null = function(cls, ...) {
  for (x in list(...)) {
    if (is.null(x)) next
    assert_class(cls, x)
  }
  return(invisible(TRUE))
}

bcrypt_hash = function(x) {
  bcrypt::hashpw(x, bcrypt::gensalt(.BCRYPT_SALT_ROUNDS))
}

where = function(xs, f, ...) {
  if (is.character(f)) return(Filter(\(x) x[[f]], xs))
  Filter(\(x) do.call(f, list(x, ...)), xs)
}

where_not = function(xs, f, ...) {
  if (is.character(f)) return(Filter(\(x) !x[[f]], xs))
  where(xs, Negate(f), ...)
}

map = function(xs, f, ...) {
  if (is.character(f)) return(Map(\(x) x[[f]], xs))
  Map(\(x) do.call(f, list(x, ...)), xs)
}

mapkv = function(m, f, ...) {
  names(m) |> map(\(k) f(k, m[[k]], ...))
}

groupby = function(xs, f, ...) {
  if (is.character(f)) return(groupby(xs, \(x) x[[f]]))
  g = \(x) f(x, ...)
  m = list()
  counter = c()
  for (x in xs) {
    k = as.character(g(x))
    if (is.null(m[[k]])) {
      m[[k]] = list()
      counter[k] = 0
    }
    counter[k] = counter[k] + 1
    m[[k]][[counter[[k]]]] = x
  }
  m
}

lupsert = function(x, y) {
  assert_class("list", x, y)
  for (z in list(x, y)) stopifnot(names(z) |> not_null() |> all())
  z = x
  for (key in names(y)) z[[key]] = y[[key]]
  z
}

count = function(x, predicate, ...) {
  x |> where(predicate, ...) |> length()
}
