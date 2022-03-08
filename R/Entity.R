Entity = function(table, decl) .Entity(table, substitute(decl))

schema = function(entity) {
  conn = .get_conn(); on.exit(DBI::dbDisconnect(conn))
  DBI::dbListFields(conn, attr(entity, "table"))
}

show_tables_sql = function(envs_above = 1) {
  for (e in .all_entities(envs_above + 1)) cat(.create_table_sql(e), "\n")
}

create_tables = function(drop_if_exists = FALSE) {
  entities = .all_entities(envs_above = 2)
  for (entity in entities) create_table(entity, drop_if_exists = drop_if_exists)
}

# private

.ENTITY_DECL = "entitydecl"
.KORAL_DBARGS = "koral_dbargs"
.BCRYPT_SALT_ROUNDS = 10

.all_entities = \(envs_above = 1) parent.frame(envs_above) |> as.list() |> where(is, .ENTITY_DECL)

.Entity = function(table, fields_quote) {
  entity = new.env()
  local(eval(fields_quote), entity)
  entity = entity |> as.list() |> where(is, .FIELD_DECL) |> add_class(.ENTITY_DECL)

  attr(entity, "get_conn") = \() do.call(DBI::dbConnect, dbargs)
  attr(entity, "table") = table

  entity
}

.has_no_pk        = \(entity) entity |> map("pk") |> not() |> all()
.has_composite_pk = \(entity) count(entity, `$`, "pk") > 1
.has_fk           = \(entity) entity |> map(.is_fk) |> as_vec() |> any()

.constraints_sql = function(entity) {
  pks = entity |> where("pk") |> names() |> mkstring(", ")
  if (!.has_fk(entity)) return(sprintf("\tPRIMARY KEY (%s)", pks))
  fks = entity |>
    where(.is_fk) |>
    groupby(\(f) attr(f$fk$entity, "table")) |>
    mapkv(function(tref, fks) {
      this = fks |> map("name") |> mkstring(", ")
      that = fks |> map(\(f) f$fk$field) |> mkstring(", ")
      sprintf("\tFOREIGN KEY (%s) REFERENCES %s (%s)", this, tref, that)
    }) |>
    mkstring(",\n")
  sprintf("%s,\n\tPRIMARY KEY (%s)", fks, pks)
}

.create_table_sql = function(entity) {
  assert_class(.ENTITY_DECL, entity)
  fields = entity |> .db_fields() |> map(.field_decl_sql) |> map(prepend, "\t") |> mkstring(",\n")
  sprintf("CREATE TABLE %s (\n%s,\n%s\n);", attr(entity, "table"), fields, .constraints_sql(entity))
}
