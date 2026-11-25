# Regional benchmark series: search record

Web of Science Core Collection, Advanced Search. Exports of 2026-11-25 for the European series and the U.S. experimental series.

## Settings applied to every export

- Document types: Article and Early Access only.
- Publication date: 1990-01-01 to 2026-07-01.
- Counts exported from Analyze Results by **Publication Years** (not "Final Publication Year", which leaves out records without a final year).
- Each series comes from a single combined query, so a paper with authors in several countries of the same region is counted once.

## Queries

`TS` (experimental design terms), identical to the main search:

```
TS=("field experiment*" OR "survey experiment*" OR "audit stud*" OR "correspondence experiment*" OR "correspondence audit*" OR "conjoint experiment*" OR "conjoint analy*" OR "conjoint stud*" OR "correspondence test*" OR "list experiment*" OR "factorial experiment*" OR "factorial survey*" OR "vignette experiment*" OR "vignette survey*" OR "correspondence test*" OR "resume audit*" OR "CV audit*" OR "résumé audit*" OR "choice experiment" OR "conjoint study" OR "online experiment")
```

`WC` (social science categories), identical to the main search:

```
WC=("Education & Educational Research" OR "Economics" OR "Management" OR "Political Science" OR "Business" OR "Social Sciences, Interdisciplinary" OR "Communication" OR "Sociology" OR "International Relations" OR "Public Administration" OR "Urban Studies" OR "Women's Studies" OR "Social Issues" OR "Social Sciences, Mathematical Methods" OR "Family Studies" OR "Development Studies" OR "Cultural Studies" OR "Demography" OR "Social Sciences, Biomedical" OR "Psychology, Multidisciplinary" OR "Psychology, Clinical" OR "Psychology, Applied" OR "Psychology, Experimental" OR "Psychology, Developmental" OR "Psychology" OR "Psychology, Social" OR "Psychology, Educational" OR "Psychology, Psychoanalysis" OR "Psychology, Biological" OR "Psychology, Mathematical")
```

`CU` for Europe (45 countries and territories above the 500-record threshold; the four United Kingdom nations are entered separately because Web of Science does not record "United Kingdom"; "Turkey" and "Turkiye" are both included):

```
CU=("Albania" OR "Austria" OR "Belarus" OR "Belgium" OR "Bosnia & Herceg" OR "Bulgaria" OR "Croatia" OR "Cyprus" OR "Czech Republic" OR "Denmark" OR "England" OR "Estonia" OR "Finland" OR "France" OR "Germany" OR "Greece" OR "Hungary" OR "Iceland" OR "Ireland" OR "Italy" OR "Kosovo" OR "Latvia" OR "Lithuania" OR "Luxembourg" OR "Malta" OR "Montenegro" OR "Netherlands" OR "North Ireland" OR "North Macedonia" OR "Norway" OR "Poland" OR "Portugal" OR "Romania" OR "Russia" OR "Scotland" OR "Serbia" OR "Slovakia" OR "Slovenia" OR "Spain" OR "Sweden" OR "Switzerland" OR "Turkey" OR "Turkiye" OR "Ukraine" OR "Wales")
```

`CU` for the United States: `CU=("USA")`.

## Series and totals

| File | Query | Total | Header check |
|---|---|---|---|
| `experimental/europa_exp.txt` | `TS AND WC AND CU(Europe)` | 12,694 | rows sum to 12,694 |
| `totales denominador/por region-año/europa_gral.txt` | `WC AND CU(Europe)` | 1,774,664 | rows sum to 1,774,664 |
| `experimental/usa_exp.txt` | `TS AND WC AND CU("USA")` | 12,334 | rows sum to 12,334 |

Not re-exported in this round: `usa_gral.txt` (1,599,037), `latam_gral.txt` (166,291) and `latam_exp.txt` (rebuilt from the validated corpus, 675 records, 611 in 1995-2025). They should be repeated on the same date.

## Table 2 values after the update (1995-2025)

| Region | Experimental | General | Per 1,000 | Experimental 2016-2025 |
|---|---|---|---|---|
| Latin America | 611 | 155,258 | 3.94 | 520 |
| United States | 11,568 | 1,447,296 | 7.99 | 8,732 |
| Europe | 11,864 | 1,659,708 | 7.15 | 9,444 |

These values were computed outside R from the three new files. The R scripts (`07_figuras.R`, `07_resultados_paper.R`) must be re-run to regenerate the tables and figures; the numbers should match.

## Series replaced

- `europa_exp.txt` (2,545 records): the annual values match the European "Other" discipline series (2,538 records), not the European total.
- `europa_gral.txt` (2,531,413 records): not reproducible with the European country list, even without filters.
- `usa_exp.txt` (11,828 records plus 478 without data): exported by "Final Publication Year".

The country tables and totals recorded earlier for Europe (search a, 1,703,183 and 1,330,887; search b, 17,314; search c by discipline) should not be used. The per-country tables count paper-country pairs, and the United Kingdom total adds its four nations.

## Pending

- Repeat `usa_gral` and `latam_gral` on the same date as the European exports.
- Test the 2021 step in the Latin American denominator (7,333 in 2020, 18,243 in 2021) with exports limited to SSCI.
- Confirm that the 500-record threshold produces the final European country list.
