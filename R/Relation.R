#' Declare the model for a relation of items.
#'
#' @param table The name of the table.
#' @param ... Sequence of `koral_field` declarations (or lists of those, to be unnested).
#'
#' @return An `koral_relation` object. It's a named list of `koral_field`s with an
#'         `"table"` attribute.
#' @export
Relation = function(table, ...) {
  fields = flatten(list(...), where = is, .FIELD_DECL)
  names  = map(fields, "name")

  fields |>
    setNames(names) |>
    orderby("deduced") |>
    add_class(.RELATION_DECL) |>
    add_attr("table", table)
}

# private

.all_relations    = function(n = 1) parent.frame(n) |> as.list() |> where(is, .RELATION_DECL)
.has_composite_pk = function(relation) count(relation, `$`, "pk") > 1
.has_fk           = function(relation) relation |> map(.is_fk) |> as_vec() |> any()

.constraints_sql = function(relation) {
  pks = relation |> where("pk") |> names() |> mkstring(", ")
  if (!.has_fk(relation)) return(sprintf("\tPRIMARY KEY (%s)", pks))
  fks = relation |>
    where(.is_fk) |>
    groupby(\(f) attr(f$fk$relation, "table")) |>
    mapkv(function(tref, fks) {
      this = fks |> map("name") |> mkstring(", ")
      that = fks |> map(\(f) f$fk$field) |> mkstring(", ")
      sprintf("\tFOREIGN KEY (%s) REFERENCES %s (%s)", this, tref, that)
    }) |>
    mkstring(",\n")
  sprintf("%s,\n\tPRIMARY KEY (%s)", fks, pks)
}

.create_table_sql = function(relation) {
  assert_class(.RELATION_DECL, relation)
  fields = relation |>
    .db_fields() |>
    map(.field_decl_sql) |>
    map(prefix_with, "\t") |>
    mkstring(",\n")
  sprintf("CREATE TABLE %s (\n%s,\n%s\n);", attr(relation, "table"), fields, .constraints_sql(relation))
}
