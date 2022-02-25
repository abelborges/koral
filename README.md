# koral

Minimalist ORM for R. Easily declare
entities, as well as their columns and constraints,
and get back the whole machinery for database interactions.

```R
library(koral)
options("koral.dbargs" = list(drv = RPostgres::Postgres()))

User = Entity("users", {
  UuidPrimaryKey("id")
  Column("name")
  Column("phone", unique = TRUE)
  Column("password", transient = TRUE)
  Column("hashed_password", deduced = TRUE, parser = \(obj) bcrypt_hash(obj$password))
  Column("instagram", nullable = TRUE)
  timestamps()
})

User$create_table()

user_dto = list(
  name = "Mary",
  phone = "00 00000 0000",
  password = "a password",
  instagram = "mary_handle"
)

User$insert(user_dto)
```
