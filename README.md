# GASImpactModel
A static cohort model was developed to estimate the projected health impact of 
group A Streptococcus (GAS) vaccination using country-specific demographic data. 
The model is available as an R package GASImpactModel 
and a user-friendly (R Shiny) web application (https://github.com/fionagi/GASImpactModel_App). 
Vaccination impact is estimated 
in terms of reduction in the burden of several major GAS disease states and sequelae. 
Burden estimation comprised of episodes of acute GAS disease (pharyngitis, impetigo, 
invasive disease, and cellulitis) and cases of rheumatic heart disease (RHD), 
deaths due to severe GAS disease (invasive disease and RHD), and disability-adjusted 
life years (DALYs) due to each GAS disease. The reduction in disease burden is in 
direct proportion to vaccine efficacy, vaccine coverage, and vaccine-derived immunity 
(based on duration of protection and waning dynamics). Through the web application, 
impact metrics for a selected vaccination scenario can be visualised for any of 205 
countries abd regions. The app shows the predicted lifetime health benefits, from age of vaccination,
associated with the vaccination of multiple cohorts during the period selected. 
Direct effects of vaccination are included, and indirect (herd) effects are excluded; 
therefore, the estimated health benefits of GAS vaccination are conservative 
(if GAS vaccination prevents population transmission).
