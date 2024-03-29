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
  "80 to 84",  0,
  "85 to 89",  0,
  "90 to 94",  0,
  "95 plus",  0
)

inc_invasive <- dplyr::tribble(
  ~age,      ~val,
  "<1 year",  0.000246,
  "1 to 4",   0.0000826,
  "5 to 9",   0.0000187,
  "10 to 14", 0.0000113,
  "15 to 19", 0.0000146,
  "20 to 24", 0.0000216,
  "25 to 29", 0.0000287,
  "30 to 34", 0.0000347,
  "35 to 39", 0.0000394,
  "40 to 44", 0.0000433,
  "45 to 49", 0.0000472,
  "50 to 54", 0.0000525,
  "55 to 59", 0.0000599,
  "60 to 64", 0.0000697,
  "65 to 69", 0.0000826,
  "70 to 74", 0.0000991,
  "75 to 79", 0.0001201,
  "80 to 84", 0.0001462,
  "85 to 89", 0.0001784,
  "90 to 94", 0.0002178,
  "95 plus",  0.0002657
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

#prob (based on age) of invasive incidents result in
#death in the first year
probDeath_invasive <- dplyr::tribble(
~Age, 	~pDeath,
"<1 year",	0.06,
"1 to 4", 	0.06,
"5 to 9", 	0.07,
"10 to 14", 0.08,
"15 to 19",	0.08,
"20 to 24",	0.09,
"25 to 29",	0.11,
"30 to 34",	0.12,
"35 to 39",	0.13,
"40 to 44",	0.15,
"45 to 49",	0.17,
"50 to 54",	0.19,
"55 to 59",	0.21,
"60 to 64",	0.24,
"65 to 69",	0.27,
"70 to 74",	0.30,
"75 to 79",	0.34,
"80 to 84",	0.38,
"85 to 89",	0.43,
"90 to 94",	0.48,
"95 to 99",	0.54
)


