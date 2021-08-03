lookup_condition <- dplyr::tribble(
        ~Condition,                    ~Label,
        "Rheumatic Heart Disease",     "rhd",
        "Cellulitis",                  "cellulitis"
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
