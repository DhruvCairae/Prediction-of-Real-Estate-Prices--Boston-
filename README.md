# Prediction-of-Real-Estate-Prices--Boston-
 	Data is derived from the paper-” Hedonic Housing Prices and the Demand for Clean Air”- written in 1978. Purpose of the research was to determine willingness to pay for clean air.
 	Hedonic housing price model and housing data for Boston Metropolitan area, are used to test the hypothesis that individuals would pay more for a unit located  in an area with good air quality as opposed to an identical unit located in an area with poor air quality
 	Data is a composition of structural, neighborhood, accessibility & air pollution specific  variables which intend to capture different pieces of information relevant to a home owner.
 	The objective is to determine mathematical relationship between median value of owner occupied homes from thirteen other variables. 
 	Model would demonstrate relative significance of air pollution to home owners, this in turn could be used as a proxy to determine willingness to pay for clean air. 
 	Structural: Average number of rooms & age in terms of proportion of units built prior to 1940
 	Neighborhood: Racial profile and economic status of neighbors, crime rate, proportion of large units, proxy for noise, heavy traffic and unpleasant visuals, tax, student-teacher ratio in schools, & proximity to Charles river. 
 	Accessibility: Distance from employment centers and access to radial highways
 	Air pollution: Nitrogen oxide and particulate concentrations.
 	Exploratory Data Analysis, correlation & visualizations variables of interest. 
 	Best fit model identified linear regression model step-wise BIC/AIC, Lasso regression
 	Final model selected~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + black + lstat
 	Two Methods Step Wise AIC & LASSO(Lamba-Min) recommend this model
 	It has the minimum Model- MSE (In Sample) 
 	From a business perspective, logically the variables are capturing different pieces of information, hence the model chosen appears apt.
 	Best fit regression tree identified and performance compared with best fit linear model. 
