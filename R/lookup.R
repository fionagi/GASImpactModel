lookup_condition <- dplyr::tribble(
        ~Condition,                    ~Label,
        "Rheumatic Heart Disease",     "rhd",
        "Cellulitis",                  "cellulitis",
        "Invasive infection",          "invasive",
        "Pharyngitis",                 "pharyngitis",
        "Impetigo",                    "impetigo",
        "Acute Rheumatic Fever",       "arf"
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
  "80 to 84",  5,
  "85 to 89",  5,
  "90 to 94",  5,
  "95 plus",  5
)

disability_weights <- dplyr::tribble(
  ~Condition,                  ~DW,
  "Rheumatic Heart Disease",   0.049,
  "Cellulitis",                0.051,
  "Invasive infection",        0.133,
  "Pharyngitis",               0.026,
  "Impetigo",                  0.006,
  "Acute Rheumatic Fever",     0.049
)

duration <- dplyr::tribble(
  ~Condition,                 ~Days,
  "Rheumatic Heart Disease",  NA, #life
  "Cellulitis",               16.4,
  "Invasive infection",       10,
  "Pharyngitis",              5,
  "Impetigo",                 15.5,
  "Acute Rheumatic Fever",    28
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
  "80 to 84",  0,
  "85 to 89",  0,
  "90 to 94",  0,
  "95 plus",  0
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
  "80 to 84",  0,
  "85 to 89",  0,
  "90 to 94",  0,
  "95 plus",  0
)
#Prevalence estimates for impetigo
prev_impetigo <- dplyr::tribble(
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
  "80 to 84",  0,
  "85 to 89",  0,
  "90 to 94",  0,
  "95 plus",  0
)

#Adjust prevalence estimates using duration to get incidence
#estimates for impetigo
inc_impetigo <- dplyr::tibble(prev_impetigo$age,
                      prev_impetigo$val/(duration$Days[duration$Condition == "Impetigo"]/365.25))
colnames(inc_impetigo) <- c("age", "val")

#Need to CHECK
probDeath_RHD <- dplyr::tribble(
~TimeSinceRHD,	~CpDeath.HIC,  ~CpDeath.LMIC, 	~pDeath.HIC,  ~pDeath.LMIC,
    0,         	0.001399175,       	0.11,       	0.001399175,  	0.11,
    1,        	0.004819936,	      0.17,       	0.003420761,  	0.06,
    2,        	0.011672587,	      0.19,	        0.006852651,	  0.0163,
    3,        	0.013507646,	      0.2,	        0.001835059,	  0.0163,
    4,        	0.015491904,	      0.22,	        0.001984258,	  0.0163,
    5,	        0.022277603,	      0.24,	        0.006785699,	  0.0163,
    6,	        0.022277603,	      0.25,       	0,            	0.0163,
    7,	        0.022277603,	      0.27,	        0,            	0.0163,
    8,	        0.02534893,	        0.28,	        0.003071327,  	0.0163,
    9,	        0.0295888,	        0.3,	        0.00423987,   	0.0163
)

#4% of invasive incidents result in death in the first year
probDeath_invasive <- dplyr::tribble(
  ~TimeSince,	~pDeath,
    0,         	0.04
)


#Ratio of ARF to RHD
ARFratio <- dplyr::tribble(
  ~age,      ~Endemic,     ~NonEndemic,
  "<1 year",   0,            0,
  "1 to 4",    2,            2.5,
  "5 to 9",    4,            3.5,
  "10 to 14",  2.5,          3.25,
  "15 to 19",  1,            1,
  "20 to 24",  1,            1,
  "25 to 29",  1,            1,
  "30 to 34",  0.5,          0.8,
  "35 to 39",  0.5,          0.6,
  "40 to 44",  0.5,          0.4,
  "45 to 49",  0,            0,
  "50 to 54",  0,            0,
  "55 to 59",  0,            0,
  "60 to 64",  0,            0,
  "65 to 69",  0,            0,
  "70 to 74",  0,            0,
  "75 to 79",  0,            0,
  "80 to 84",  0,            0,
  "85 to 89",  0,            0,
  "90 to 94",  0,            0,
  "95 plus",  0,            0
)

#Progression from ARF to RHD (cols) for each year after 0 vax efficacy (rows);
# interpolation of cumulative incidence data from https://doi.org/10.1161/CIRCULATIONAHA.115.020966
ARFprog <- dplyr::tribble(
  ~TimeSince0vaxEff, ~TimeSinceARF_9y, ~TimeSinceARF_8y, ~TimeSinceARF_7y, ~TimeSinceARF_6y,
  ~TimeSinceARF_5y, ~TimeSinceARF_4y, ~TimeSinceARF_3y, ~TimeSinceARF_2y, ~TimeSinceARF_1y,
  1, 0.01588, 0.01588, 0.01588, 0.01588, 0.01588, 0.04215, 0.04215, 0.04215, 0.04215,
  2, 0.01588, 0.01588, 0.01588, 0.01588, 0.01588, 0.04215, 0.04215, 0.04215, 0.00000,
  3, 0.01588, 0.01588, 0.01588, 0.01588, 0.01588, 0.04215, 0.04215, 0.00000, 0.00000,
  4, 0.01588, 0.01588, 0.01588, 0.01588, 0.01588, 0.04215, 0.00000, 0.00000, 0.00000,
  5, 0.01588, 0.01588, 0.01588, 0.01588, 0.01588, 0.00000, 0.00000, 0.00000, 0.00000,
  6, 0.01588, 0.01588, 0.01588, 0.01588, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
  7, 0.01588, 0.01588, 0.01588, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
  8, 0.01588, 0.01588, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
  9, 0.01588, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000
)

