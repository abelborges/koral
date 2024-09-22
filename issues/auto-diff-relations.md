The idea is to follow [django](https://docs.djangoproject.com/en/4.1/topics/migrations/) in that most things are automated.

The user point of view is:

1.  changes database model code
2.  runs `koral::commit_migration`

So migrations are stateful operations that are given 3 inputs:

1.  `name`: short description of the transformation
2.  `parent_id`: `id` of the base migration
3.  the final state of the database model

Parameters 2 and 3 are passed implicitly: `parent_id` is taken from the migrations directory and it identifies the origin database model; the destination model is deduced from code.

Then other fields are deduced:

-   `id`: string with the format `{counter}_{name}`, e.g. `1_init`, `2_adds_timestamps`; the counter starts at 1

-   `content_hash`: hash of the migration content

-   `koral_version`: two migrations with the same version can be identified by `content_hash` whereas

Desired properties:

-   If two migrations have the same `parent_id` and `content_hash`, this should signal that they lead to the same state.

-   On the other hand, differences in `content_hash` may be due to both semantics (in user space) or evolution of the internal migration representation, so this does not deterministically signals that two migrations lead to different states.

-   Migration files are R code, though it should be easy to pretty-print the actual SQL on-demand.
