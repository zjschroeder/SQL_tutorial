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
| Start here | what a relational DB is; vocabulary (statement, clause, predicate; DQL/DML/DDL/TCL/DCL); the schema; written order vs execution order |
| SELECT | projecting columns, `SELECT *`, `AS`, `DISTINCT`, computed columns |
| WHERE | comparisons, `AND/OR/NOT`, `BETWEEN`, `IN`, `LIKE` (`ESCAPE`, `GLOB`), **`NULL` and three-valued logic** |
| ORDER BY | sorting, tiebreakers, `NULLS FIRST/LAST`, `LIMIT`/`OFFSET`, top-N |
| GROUP BY | `COUNT`/`SUM`/`AVG`/`MIN`/`MAX`, `COUNT(DISTINCT)`, `GROUP_CONCAT`, `HAVING` vs `WHERE` |
| Functions | `\|\|` and text functions, `ROUND`, integer division, `CAST`, **`CASE`**, `COALESCE`/`NULLIF`, conditional aggregation and `FILTER`, dates |
| JOINs | `INNER`/`LEFT`/`RIGHT`/`FULL OUTER`/`CROSS`, `USING`, self-joins, **row multiplication**, the LEFT-to-INNER filter trap |
| UNION | `UNION ALL`, `UNION`, `INTERSECT`, `EXCEPT`, and the rules for combining results |
| Subqueries | scalar/`IN`/`EXISTS`/correlated subqueries, the `NOT IN` + NULL trap, CTEs (`WITH`), derived tables, `WITH RECURSIVE` |
| Windows | `OVER`, `PARTITION BY`, ranking, running totals, `LAG`/`LEAD`, frames (`ROWS BETWEEN`), top-N per group, `NTILE`, `FIRST_VALUE` |
| Changing data | `INSERT`/`UPDATE`/`DELETE`, upsert (`ON CONFLICT`), `RETURNING`, transactions (`BEGIN`/`COMMIT`/`ROLLBACK`), on a private disposable copy |
| Tables | `CREATE TABLE`, types and affinity, constraints (`PRIMARY KEY`, `NOT NULL`, `UNIQUE`, `CHECK`, `DEFAULT`), foreign keys, `ALTER`/`DROP TABLE`, views, `CREATE TABLE AS` |
| Put it together | 20 mixed challenges, a free sandbox, a keyword index, what other dialects add, running SQL from R, a note on indexing and speed |

Each walkthrough tab is a short sequence of concept steps that ends with a
**10-question live-editor "quest"**. Ten is not a magic number; it is
roughly how many it takes before a clause stops feeling foreign.

The execution-order ribbon (FROM / JOIN, WHERE, GROUP BY, HAVING, window
functions, SELECT, DISTINCT, UNION / INTERSECT / EXCEPT, ORDER BY,
LIMIT / OFFSET) recurs through the tabs. Every chip on it is taught, and
lit, on some tab; `R/sql_helpers.R` lists where.

## Accuracy notes

The app runs SQLite, and says so where SQLite departs from the standard
or from the other major databases. The departures that matter to a
learner, all covered in the text: SQLite lets `WHERE` and `HAVING` use a
`SELECT` alias; it accepts a double-quoted string where an identifier
was expected; its column types are advisory unless the table is
`STRICT`; foreign keys are enforced only after `PRAGMA foreign_keys = ON`;
it permits bare columns in an aggregate query; and it evaluates chained
set operators strictly left to right. `RIGHT` and `FULL OUTER JOIN` need
SQLite 3.39+, `RETURNING` and `DROP COLUMN` 3.35+; these appear only in
ungraded demos or on the writing tabs, never in a read-only quest's
reference query.

## How the live editor works

- Query tabs use a **read-only** connection to `data/institute.sqlite`,
  so a stray `DROP` or `DELETE` cannot harm the data. The editor
  politely declines data-modifying statements until the Changing-data
  tab.
- The Changing-data and Tables tabs share a **private in-memory copy**
  per session, with a Reset button. The copy is rebuilt from the
  `CREATE` statements in `sqlite_master`, so it keeps the primary keys,
  `NOT NULL`s, defaults, foreign keys, and indexes of the real thing
  (`dbWriteTable()` would drop them all). Tasks there are graded by
  inspecting the copy's state after your statement has run.
- Grading runs a reference query against the same database and compares
  result sets (order-insensitive unless the question is about ordering),
  so the answer key cannot drift from the data. Every order-sensitive
  question names a tiebreaker, because SQL leaves the order of tied rows
  undefined.

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
