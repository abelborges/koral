options("koral_dbargs" = list(
  RPostgres::Postgres(),
  host = 'localhost',
  user = 'postgres',
  port = 5432
))

devtools::load_all()

User = Relation("users",
                UuidPrimaryKey("id"),
                StringField("name"),
                StringField("phone", unique=T),
                StringField("password", parser = function(dto) bcrypt_hash(dto$password)),
                EnumField("tags", values = c("a", "b")),
                timestamps()
)

User2 = Relation("users",
                 UuidPrimaryKey("id"),
                 StringField("name"),
                 StringField("phone"),
                 StringField("password", parser = function(dto) bcrypt_hash(dto$password)),
                 EnumField("tags", values = c("a", "b", "c")),
                 timestamps()
)

Blocklist = Relation("blocklist",
                     UuidField("user_id", pk=T, fk = ForeignKey(User)),
                     timestamps()
)

show_tables_sql()
create_table(User, drop_if_exists=T)
create_table(Blocklist, drop_if_exists=T)

constraints = list(
  pk     = function(tablename)            paste0(tablename,             "_pkey"),
  fk     = function(tablename, fieldname) paste0(tablename, "_", fieldname, "_fkey"),
  enum   = function(tablename, fieldname) paste0(tablename, "_", fieldname, "_check"),
  unique = function(tablename, fieldname) paste0(tablename, "_", fieldname, "_key")
)

field_diffs = list(
  pk = function(orig, dest, tablename) {},

  fk = function(orig, dest, tablename) {},

  nullable = function(orig, dest, tablename) {
    if (orig$nullable == orig$nullable) return(NULL)

    drop = orig$nullable
    paste(
      "ALTER TABLE", tablename,
      "ALTER COLUMN", orig$name,
      if (drop) "DROP" else "SET", "NOT NULL"
    )
  },

  unique = function(orig, dest, tablename) {
    if (orig$unique == orig$unique) return(NULL)

    drop = begin$unique
    paste(
      "ALTER TABLE", tablename,
      if (drop) "DROP" else "ADD",
      "CONSTRAINT", constraints$unique(tablename, fieldname),
      if (drop) "" else paste0("UNIQUE (", fieldname, ")")
    )
  },

  enum = function(orig, dest, tablename) {

  }
)

# field is kept but with possibly new spec
diff_fields = function(orig, dest, tablename, ...) {
  assert_class(.FIELD_DECL, orig, dest)
  assert_equals(orig$name, dest$name)
  Reduce(c, c(field_diffs, list(...)), init = NULL)
}

diff_fields(User$tags, User2$tags, "users")

# handle removed fields, otherwise call diff_fields
diff_relations = function(begin, final) {
  assert_class(.RELATION_DECL, begin, final)
  assert_equals(attr(begin, "table"), attr(final, "table"))

  dropped = setdiff(names(begin), names(final))
  added   = setdiff(names(final), names(begin))
  kept    = intersect(names(final), names(begin))

}

withdb = function(f) {
  db = .get_conn(); on.exit(DBI::dbDisconnect(db))
  if (is.character(f)) return(DBI::dbGetQuery(db, f))
  f(db)
}

get_constraints = function(table) {
  query = sprintf("
  SELECT con.conname as constraint
  FROM
    pg_catalog.pg_constraint con
    INNER JOIN pg_catalog.pg_class rel ON rel.oid = con.conrelid
    INNER JOIN pg_catalog.pg_namespace nsp ON nsp.oid = connamespace
  WHERE nsp.nspname = 'public'
  AND rel.relname = '%s';", table)
  withdb(query)
}

get_constraints("users")
