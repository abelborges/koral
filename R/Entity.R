.ENTITY_DECL = "entitydecl"
.KORAL_DBARGS = "koral.dbargs"
.BCRYPT_SALT_ROUNDS = 10

.Entity = function(table, dbargs, columns_quote) {
  entity = new.env()
  local(eval(columns_quote), entity)
  entity = entity |>
    as.list() |>
    where(is, .COLUMN_DECL) |>
    add_class(.ENTITY_DECL)
  entity$.table = table

  get_conn = \() do.call(DBI::dbConnect, dbargs)

  entity$create_table = function() {
    conn = get_conn(); on.exit(DBI::dbDisconnect(conn))
    create_table(conn, entity)
  }

  entity$drop_table = function() {
    conn = get_conn(); on.exit(DBI::dbDisconnect(conn))
    if (DBI::dbExistsTable(conn, table)) DBI::dbRemoveTable(conn, table)
  }

  entity$insert = function(x) {
    conn = get_conn(); on.exit(DBI::dbDisconnect(conn))
    instance = from_input(entity, x)
    insert_into(conn, entity, instance)
  }

  # entity$update = function(x) {
  #   conn = get_conn(); on.exit(DBI::dbDisconnect(conn))
  #   instance = from_input(entity, x)
  #   updatable_fields = fields |> where("updatable") |> map("name")
  #   insert_into(conn, entity, pick(instance, updatable_fields))
  # }

  entity
}

Entity = function(table, columns) {
  dbargs = getOption(.KORAL_DBARGS)
  if (is.null(dbargs)) stop(glue::glue("R option {.KORAL_DBARGS} should be set"))
  .Entity(table, dbargs, substitute(columns))
}
