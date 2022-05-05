utctime = function() lubridate::now("UTC")
utcdate = function() lubridate::date(utctime())

prefix_with = function(x, prefix) paste0(prefix, x)
rm_double_spaces = function(x) gsub("\\s+", " ", trimws(x))

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

not = function(x) !x
not_null = function(x) !is.null(x)
is_scalar = function(x) is.atomic(x) && length(x) == 1L
is_non_null_scalar = function(x) if (!is.list(x) || !is.vector(x)) FALSE else !is.null(x) && is_scalar(x)
is_non_null_string = function(x) is_non_null_scalar(x) && is.character(x)
is_non_null_number = function(x) is_non_null_scalar(x) && is.numeric(x)
is_positive_number = function(x) is_non_null_number(x) && x > 0
add_class = function(x, cls) structure(x, class = cls)
add_attr = function(x, k, v) { attr(x, k) = v; x }
env_as = function(cls, include_private = FALSE) as.list(parent.frame(), all.names = include_private) |> add_class(cls)
as_vec = function(x) unlist(x, recursive = TRUE, use.names = FALSE)
first_scalar = function(x) as_vec(x)[[1]]
is_map = function(x) is.list(x) && !is.null(names(x))

assert_class = function(cls, ...) {
  for (x in list(...)) {
    if (!is(x, cls)) stop(paste("class of", x, "is not", cls))
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

orderby = function(xs, f, ...) {
  ordered_indices = xs |> map(f) |> unlist() |> order()
  xs[ordered_indices]
}

lupsert = function(x, y) {
  assert_class("list", x, y)
  for (z in list(x, y)) stopifnot(names(z) |> not_null() |> all())
  z = x
  for (key in names(y)) z[[key]] = y[[key]]
  z
}

concat_unnamed_lists = function(x, y) {
  z = x
  counter = length(z)
  for (o in y) {
    counter = counter + 1
    z[[counter]] = o
  }
  z
}

# given a list of lists (ignores non-lists), if one of them is not a map (i.e. has no
# names) recursively unnests looking for maps to add into the returned list
flatten = function(input, where = is_map, ...) {
  predicate = \(x) where(x, ...)

  output = list()
  counter = 0

  for (i in seq_along(input)) {
    x = input[[i]]

    if (predicate(x)) {
      counter = counter + 1
      output[[counter]] = x
      next
    }

    if (!is.list(x)) next

    output = concat_unnamed_lists(output, flatten(x, predicate))
  }

  output
}

count = function(x, predicate, ...) {
  x |> where(predicate, ...) |> length()
}
