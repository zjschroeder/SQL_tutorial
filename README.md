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

## The second database: a school district

**Prairie Lakes Public Schools** (`data/district.sqlite`) is a fictional
Minnesota-style district whose tables follow the shape of a real student
information system (SIS). Nine schools (four elementary, two middle, two
high, one area learning center), five school years (2021-22 to 2025-26),
and a fixed "today" of **2026-03-31**, part-way through the current year.
Every person, name, and number is invented.

| Table | One row is… | Notable for teaching |
|------------------------|------------------------|------------------------|
| `schools` | a school | `state_school_number` is the state's key, not the district's |
| `students` | a person | `state_id` is text with leading zeros; four are `NULL`; race and ethnicity as flags |
| `enrollments` | an enrollment spell | start/end dates (`end_date` NULL = still enrolled), status codes, primary vs. secondary service |
| `calendar_days` | a weekday at a school | `instructional` = 1 on school days; the denominator for attendance |
| `attendance` | an absence or tardy (current year) | exceptions only: no row means present |
| `attendance_year_summary` | a student-year at a school (prior years) | frozen year-end membership and absence days |
| `programs` | a program spell | EL, SPED, 504, FRL, McKV, GT, each with dates |
| `incidents`, `incident_students`, `resolutions` | an event / a person in it / a consequence | the three-way fan-out |
| `courses`, `sections`, `roster`, `staff` | catalog / class / enrollment in a class / staff | 2025-26 high school schedules |
| `transcripts` | a final mark for a term | credits toward the 21.5 needed to graduate; transfer and converted credit |
| `test_results` | a state test record | non-tested students have a row with a NULL score |
| `oct1_snapshot` | a student as counted on October 1 | frozen, so it disagrees with corrected live data |
| `state_oct1_return` | a record the state accepted | reconciliation on the state's identifiers |
| `user_access` | a user's data scope | row-level security in miniature |
| `grade_levels`, `status_codes`, `attendance_codes`, `resolution_codes` | lookups | `status_codes` has a two-column key |

The data has realistic problems planted in it on purpose (overlapping and
gapped enrollments, duplicate students, an invalid grade code, missing exit
codes, a reversed date, absences on a holiday, a state file that disagrees);
`data/district_frozen/PLANTED_PROBLEMS.txt` lists them.

Rebuild with `Rscript data/build_district.R`, which reads the frozen CSVs in
`data/district_frozen/`. `data/generate_district.R` regenerates those CSVs
from the simulation; run it only to change the data, then run
`tests/verify_claims.R` and fix any sentence it flags.

## Tabs

| Tab | Teaches |
|------------------------------------|------------------------------------|
| Start here | what a relational DB is; vocabulary (statement, clause, predicate; DQL/DML/DDL/TCL/DCL); the schema; written order vs execution order |
| SELECT | projecting columns, `SELECT *`, looking up tables and columns (`sqlite_master`, `pragma_table_info`), `AS`, `DISTINCT`, computed columns |
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
| Glossary | every term in the keyword and district topic indexes, plus core ideas: a plain definition, the tab that teaches it (a link), an example, and the SQL Server spelling; searchable and filterable by category (`R/glossary.R`) |

Every tab from SELECT to Put it together also ends with an **In a school
district** section: the tab's clause applied to the district database, a
T-SQL (SQL Server) version where the dialects differ, often an interview
note, and five graded district questions (five tasks on a private district
copy for the two writing tabs). The text and quests for these live in
`R/district_sections.R`. Each section uses only its own tab's syntax and what
came before it; a T-SQL note that needs later syntax says so with a
**Preview** badge (`tsql_note(preview = ...)`).

### School district menu

| Tab | Teaches |
|------------------------------------|------------------------------------|
| District data | the grain of each SIS table; local vs. state IDs and leading zeros; enrollment spells, status codes, service types; the calendar and the as-of date; exceptions-only tables; SQLite vs. T-SQL |
| Enrollment counts | point-in-time and cumulative counts; primary enrollments and `DISTINCT`; scaffolds for zero cells; live data vs. frozen snapshot; student groups as of a date; ADM |
| Attendance | expanding enrollments over the calendar; what counts as absent; per-student rates; attendance rate vs. consistent attendance vs. chronic absence; the 90% boundary; groups and years; finding students who stopped coming |
| Discipline and groups | incidents vs. students vs. resolutions; the fan-out; unduplicated rates; federal and Minnesota race/ethnicity rules; risk ratios; small-cell and complementary suppression |
| Cohorts and outcomes | cohort definition; last enrollment per student; exit codes to outcomes; the four-year adjusted cohort graduation rate; credits on track; proficiency denominators; matched growth |
| Data quality | overlaps (self-join), gaps (`LEAD`), duplicates, invalid codes (anti-join), misplaced records, reconciliation with the state (`EXCEPT` both ways), an exception report filtered by `user_access` |
| District challenges | 15 requests written as they arrive, a district sandbox, six talk-it-through interview scenarios, connecting to SQL Server from R |

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

## Tests

``` r
Rscript tests/verify_content.R   # every quest, demo, task, index, and route (both databases)
Rscript tests/verify_claims.R    # every number the prose quotes (institute, then district)
python3 tests/scan_copy.py       # a mechanical pass over the prose
```

`tests/district_claims.R` holds the district's claims. Section 9 of
`verify_content.R` covers the district: each `*_DQUEST` runs on
`district.sqlite`, each `dsql_code()` block and each `dz…` editor starter
runs there, and the `*_DTASKS` writing tasks pass on a fresh district copy and
still pass after every district demo and task has run, in either order.
Section 9b fails if a district section's code (demos, editors, quest answers)
uses syntax from a later tab. Section 10 checks the glossary: every indexed
term has an entry, every entry links to a real tab, and its runnable examples run.

## How the live editor works

- Each editor runs on one database: the institute, or (editors whose id
  starts with `dz`, shown with a green "District database" label) the
  district. Graded answers are computed on the first Check rather than at
  startup, since some district reference queries scan the attendance table.
- Query tabs use a **read-only** connection to `data/institute.sqlite`,
  so a stray `DROP` or `DELETE` cannot harm the data. The editor
  politely declines data-modifying statements until the Changing-data
  tab.
- The Changing-data and Tables tabs share a **private in-memory copy**
  per session, with a Reset button. The district sections of those two tabs
  share a second private copy, of the district, with its own Reset. The copy is rebuilt from the
  `CREATE` statements in `sqlite_master`, so it keeps the primary keys,
  `NOT NULL`s, defaults, foreign keys, and indexes of the real thing
  (`dbWriteTable()` would drop them all). Tasks there are graded by
  inspecting the copy's state after your statement has run.
- Tab indents (two spaces, or every selected line), Shift+Tab outdents, and
  Enter keeps the current line's indentation. Press Esc, then Tab, to move
  focus out of an editor. The handler is `R/editor_keys.R`, added once to the
  page head.
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
