# koral

Minimalist ORM for R. Easily declare
entities, as well as their fields and constraints,
and get back the whole machinery for database interactions.
As of now, it's focused on the
[RPostgres](https://github.com/r-dbi/RPostgres) [DBI](https://github.com/r-dbi/DBI)
back-end, for [PostgreSQL](https://www.postgresql.org/) databases.

```R
library(koral)
options("koral_dbargs" = list(drv = RPostgres::Postgres()))

User = Entity("users", {
  UuidPrimaryKey("id")
  StringField("name")
  StringField("phone", unique = TRUE)
  StringField("password", transient = TRUE)
  StringField("hashed_password", deduced = TRUE, parser = \(dto) bcrypt_hash(dto$password))
  StringField("instagram", nullable = TRUE)
  timestamps()
})

Product = Entity("products", {
  UuidPrimaryKey("id")
  EnumField("type", values = c("a", "b", "c"))
})

Favorite = Entity("user_favorite_products", {
  UuidField("user_id", pk = TRUE, fk = ForeignKey(User))
  UuidField("product_id", pk = TRUE, fk = ForeignKey(Product))
})

# check SQL for table creation
show_tables_sql()

# create declared tables (forcefully)
create_tables(drop_if_exists = TRUE)

# insert data and get back saved instance
user = User |> insert(list(
  name = "Mary",
  phone = "00 00000 0000",
  password = "a password",
  instagram = "mary_handle"
))

product = Product |> insert(list(type = "a"))

favorite = Favorite |> insert(list(user_id = user$id, product_id = product$id))
```

The following [RStudio snippet](https://support.rstudio.com/hc/en-us/articles/204463668-Code-Snippets-in-the-RStudio-IDE)
may save some typing when creating such entities:

```
snippet ntt
	${1:name} = Entity("${2:table}", {
		${0}
	})
```
