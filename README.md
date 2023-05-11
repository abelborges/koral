# koral

⚠️ This is an exercise at the moment, use at your own discretion.

Postgres data mapping helpers for R. Easily declare
entities, as well as their fields and constraints,
and get back the whole machinery for database interactions.
As of now, it's focused on the
[RPostgres](https://github.com/r-dbi/RPostgres) [DBI](https://github.com/r-dbi/DBI)
back-end, for [PostgreSQL](https://www.postgresql.org/) databases.

```R
library(koral)
options("koral_dbargs" = list(drv = RPostgres::Postgres()))

User = Relation("users",
  UuidPrimaryKey("id"),
  StringField("name"),
  StringField("phone", unique = TRUE),
  StringField("password", transient = TRUE),
  StringField("hashed_password", deduced = TRUE, parser = \(dto) bcrypt_hash(dto$password)),
  StringField("instagram", nullable = TRUE),
  timestamps()
)

Product = Relation("products",
  UuidPrimaryKey("id"),
  EnumField("type", values = c("a", "b", "c"))
)

Favorite = Relation("user_favorite_products",
  UuidField("user_id", pk = TRUE, fk = ForeignKey(User)),
  UuidField("product_id", pk = TRUE, fk = ForeignKey(Product))
)

# check SQL for table creation
show_tables_sql()

# create declared tables (forcefully)
create_tables(drop_if_exists = TRUE)

# insert data and get back saved instance
user_dto = list(
  name = "Mary",
  phone = "00 00000 0000",
  password = "a password",
  instagram = "mary_handle"
)
user = insert(User, user_dto)

product = insert(Product, list(type = "a"))
insert(Product, list(type = "d")) # fails
insert(Favorite, list(user_id = user$id, product_id = product$id))

# fetch all or filtered-by-fields records
get_all(User)
get_one(User, phone = user$phone)
get_all(Product, type = "a")
```

The following [RStudio snippet](https://support.rstudio.com/hc/en-us/articles/204463668-Code-Snippets-in-the-RStudio-IDE)
may save some typing:

```
snippet rel
  ${1:handler} = Relation({
		${0}
		
		"${2:table}"
	})
```
