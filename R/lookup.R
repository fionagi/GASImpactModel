lookup_condition <- dplyr::tribble(
        ~Condition,                    ~Label,
        "Rheumatic Heart Disease",     "rhd",
        "Cellulitis",                  "cellulitis",
        "Invasive infection",          "invasive",
        "Pharyngitis",                 "pharyngitis",
        "Impetigo",                    "impetigo"
)

age_groups <- dplyr::tribble(
  ~Label,      ~Years,
  "<1 year",   1,
  "1 to 4",    4,
  "5 to 9",    5,
  "10 to 14",  5,
  "15 to 19",  5,
  "20 to 24",  5,
  "25 to 29",  5,
  "30 to 34",  5,
  "35 to 39",  5,
  "40 to 44",  5,
  "45 to 49",  5,
  "50 to 54",  5,
  "55 to 59",  5,
  "60 to 64",  5,
  "65 to 69",  5,
  "70 to 74",  5,
  "75 to 79",  5,
  "80 to 84",   5
)

disability_weights <- dplyr::tribble(
  ~Condition,                  ~DW,
  "Rheumatic Heart Disease",   0.049,
  "Cellulitis",                0.051,
  "Invasive infection",        0.133,
  "Pharyngitis",               0.026,
  "Impetigo",                  0.006
)

duration <- dplyr::tribble(
  ~Condition,                 ~Days,
  "Rheumatic Heart Disease",  NA, #life
  "Cellulitis",               16.4,
  "Invasive infection",       10,
  "Pharyngitis",              5,
  "Impetigo",                 15.5
)

#22.1 episodes per 100 persons per year (5-14yrs)
inc_pharyngitis <- dplyr::tribble(
  ~age,      ~val,
  "<1 year",   0,
  "1 to 4",    0,
  "5 to 9",    22.1/100,
  "10 to 14",  22.1/100,
  "15 to 19",  0,
  "20 to 24",  0,
  "25 to 29",  0,
  "30 to 34",  0,
  "35 to 39",  0,
  "40 to 44",  0,
  "45 to 49",  0,
  "50 to 54",  0,
  "55 to 59",  0,
  "60 to 64",  0,
  "65 to 69",  0,
  "70 to 74",  0,
  "75 to 79",  0,
  "80 to 84",  0
)

#CHECK
inc_invasive <- dplyr::tribble(
  ~age,      ~val,
  "<1 year",   exp(-9),
  "1 to 4",    exp(-9),
  "5 to 9",    exp(-10.7),
  "10 to 14",  exp(-11.8),
  "15 to 19",  exp(-11.7),
  "20 to 24",  exp(-10.7),
  "25 to 29",  0,
  "30 to 34",  0,
  "35 to 39",  0,
  "40 to 44",  0,
  "45 to 49",  0,
  "50 to 54",  0,
  "55 to 59",  0,
  "60 to 64",  0,
  "65 to 69",  0,
  "70 to 74",  0,
  "75 to 79",  0,
  "80 to 84",  0
)
#CHECK - estimates are per population, not 100,000 population
inc_impetigo <- dplyr::tribble(
  ~age,      ~val,
  "<1 year",   0.0008195,
  "1 to 4",    0.0014657,
  "5 to 9",    0.0012735,
  "10 to 14",  0.0009639,
  "15 to 19",  0.0004,
  "20 to 24",  0.0001245,
  "25 to 29",  0,
  "30 to 34",  0,
  "35 to 39",  0,
  "40 to 44",  0,
  "45 to 49",  0,
  "50 to 54",  0,
  "55 to 59",  0,
  "60 to 64",  0,
  "65 to 69",  0,
  "70 to 74",  0,
  "75 to 79",  0,
  "80 to 84",  0
)

