# koral

Minimalist ORM for R. Easily declare
entities, as well as their fields and constraints,
and get back the whole machinery for database interactions.
As of now, it's focused on the PostgreSQL back-end.

```R
library(koral)
options("koral.dbargs" = list(drv = RPostgres::Postgres()))

User = Entity("users", {
  UuidPrimaryKey("id")
  StringField("name")
  StringField("phone", unique = TRUE)
  StringField("password", transient = TRUE)
  StringField("hashed_password", deduced = TRUE, parser = \(dto) bcrypt_hash(dto$password))
  StringField("instagram", nullable = TRUE)
  timestamps()
})

User |> create_table(drop_if_exists = TRUE)

user_dto = list(
  name = "Mary",
  phone = "00 00000 0000",
  password = "a password",
  instagram = "mary_handle"
)

User |> insert(user_dto)
```
