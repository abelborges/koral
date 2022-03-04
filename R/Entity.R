Entity = function(table, decl) {
  dbargs = getOption(.KORAL_DBARGS)
  if (is.null(dbargs)) stop(glue::glue("R option {.KORAL_DBARGS} should be set"))
  .Entity(table, substitute(decl), dbargs)
}

# private

.ENTITY_DECL = "entitydecl"
.KORAL_DBARGS = "koral.dbargs"
.BCRYPT_SALT_ROUNDS = 10

.Entity = function(table, fields_quote, dbargs = list()) {
  entity = new.env()
  local(eval(fields_quote), entity)
  entity = entity |> as.list() |> where(is, .FIELD_DECL) |> add_class(.ENTITY_DECL)

  attr(entity, "get_conn") = \() do.call(DBI::dbConnect, dbargs)
  attr(entity, "table") = table

  entity
}
