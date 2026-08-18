---
editor_options: 
  markdown: 
    wrap: 72
---

# Step by Step SQL (Shiny)

A browser-only Shiny app that teaches SQL to people who are familiar
with data frames/coding. Every tab teaches one piece of a query (built
on SQLite), builds it one clause at a time, then hands you a **live SQL
editor** to run real queries against a real (fake) database. As it runs
on webR, there's nothing to install locally.

## The running database

**The Institute for Highly Questionable Research**: a fictional
institute whose six labs have spent decades on questions nobody asked.
Six linked tables:

| Table | One row is… | Notable for teaching |
|------------------------|------------------------|------------------------|
| `labs` | a research lab | one `NULL` building (a lab between buildings) |
| `researchers` | a person | one `NULL` `lab_id` (a visiting fellow), two `NULL` salaries (undisclosed) |
| `studies` | a study | links to a lab and a lead researcher; one abandoned |
| `participants` | a human, seagull, or cricket | eight named Dave; some `NULL` ages |
| `enrollments` | a participant in a study | the many-to-many bridge |
| `measurements` | one trial of one enrollment | repeated trials, for window functions |

Rebuild the database with `Rscript data/build_db.R` (it reads frozen
CSVs for the random tables, so the build is reproducible).

## Tabs

| Tab | Teaches |
|------------------------------------|------------------------------------|
| Start here | what a relational DB is; the schema; the execution-order idea |
| SELECT | projecting columns, `SELECT *`, `AS`, `DISTINCT`, computed columns |
| WHERE | filtering, `AND/OR/NOT`, `BETWEEN`, `IN`, `LIKE`, **`NULL` and three-valued logic** |
| ORDER BY | sorting, `LIMIT`/`OFFSET`, top-N |
| GROUP BY | aggregation, `HAVING` vs `WHERE` |
| JOINs | `INNER`/`LEFT`/self/multi-table, **row multiplication**, the LEFT-to-INNER filter trap |
| Subqueries | scalar/`IN`/`EXISTS`/correlated subqueries, CTEs (`WITH`) |
| Windows | `OVER`, `PARTITION BY`, ranking, running totals, `LAG`/`LEAD` |
| Changing data | `INSERT`/`UPDATE`/`DELETE` on a private disposable copy |
| Put it together | 20 mixed challenges, a free sandbox, a note on indexing and speed |

Each query tab is a short sequence of concept steps that ends with a
**10-question live-editor "quest"**. Ten is not a magic number; it is
roughly how many it takes before a clause stops feeling foreign.

## How the live editor works

- Query tabs use a **read-only** connection to `data/institute.sqlite`,
  so a stray `DROP` or `DELETE` cannot harm the data. The editor
  politely declines data-modifying statements until the Changing-data
  tab.
- That tab gives each session a **private in-memory copy** to modify
  freely, with a Reset button.
- Grading runs a reference query against the same database and compares
  result sets (order-insensitive unless the question is about ordering),
  so the answer key cannot drift from the data.

## Running locally

``` r
install.packages(c("shiny", "bslib", "DBI", "RSQLite"))
shiny::runApp(".")
```

### CRAN dependencies

| Package | Used for                                                          |
|--------------|----------------------------------------------------------|
| shiny   | app framework                                                     |
| bslib   | theming (`bs_theme()`, Google Fonts); Bootswatch "minty"          |
| DBI     | database interface                                                |
| RSQLite | the embedded SQLite engine (incl. its WASM build for the browser) |
