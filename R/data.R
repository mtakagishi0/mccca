#' Meal and Drink preference data
#'
#' Artificial data containing active variables (meal and drink) and external
#' variables (nationality and gender). Columns 1-2 are the active variables
#' and columns 3-4 are the external variables; select them with, e.g.,
#' \code{dat.act <- mealDrink[, 1:2]} and \code{dat.ext <- mealDrink[, 3:4]}.
#'
#' @format A data frame with 390 individuals (rows) and 4 variables (columns):
#' \describe{
#'  \item{meal}{meal preference (active variable)}
#'  \item{drink}{drink preference (active variable)}
#'  \item{nationality}{nationality (external variable)}
#'  \item{gender}{gender (external variable)}
#' }
#' @source Artificial data generated for the numerical illustration in
#' Section 3 of Takagishi, M. and van de Velden, M. (2022),
#' Visualizing class specific heterogeneous tendencies in categorical data.
#' \emph{Journal of Computational and Graphical Statistics}, 31(3), pp.790-801
"mealDrink"

#' Accident data
#'
#' A dataset obtained from the U.K. Department for Transport's road safety
#' statistics. Columns 1-4 are the active variables and columns 5-6 are the
#' external variables; select them with, e.g., \code{dat.act <- accident[, 1:4]}
#' and \code{dat.ext <- accident[, 5:6]}.
#'
#' @format A data frame with 3026 individuals (rows) and 6 variables (columns):
#' \describe{
#'  \item{Light_conditions}{4 categories...L0(Daylight), L1 (Darkness:street lights present and lit), L2 (Darkness:street lights present but unlit), L3(Darkness:no street lighting)}
#'  \item{Weather_conditions}{8 categories...Fine,Rain,Snow,Fine_w,Rain_w,Snow_w,Fog,Other.}
#'  \item{Road_Surface_conditions}{5 categories...Dry,Wet,Snow,Frost,Flood.}
#'  \item{Speed_limit}{2 categories...S30 (Speed limit is up to 30km/h), S70 (Speed limit is up to 70km/h)}
#'  \item{Casualty_Class}{2 categories..Driver (Casualty is one driver), Ped (Casualty is one pedestrian).}
#'  \item{Area}{2 categories...Urban (Occurring in urban area), Rural (Occurring in rural area). }
#' }
#' @source Data obtained from the U.K. Department for Transport's road
#' safety statistics. The subset used here consists of accidents that
#' occurred in January 2016, involved one casualty (a driver or a
#' pedestrian), and in which at most two parties were involved (N = 3026),
#' as described in Section 5.1 of Takagishi and van de Velden (2022),
#' \emph{Journal of Computational and Graphical Statistics}, 31(3), pp.790-801.
#' \url{https://www.gov.uk/government/collections/road-accidents-and-safety-statistics}
"accident"


#' Young People Survey data
#'
#' A subset of the Kaggle "Young People Survey" dataset, a self-reported
#' survey of young people in Slovakia, containing active variables (spending
#' habits) and external variables (gender and village-town). Columns 1-3 are
#' the active variables and columns 4-5 are the external variables; select
#' them with, e.g., \code{dat.act <- young[, 1:3]} and
#' \code{dat.ext <- young[, 4:5]}.
#'
#' @format A data frame with 993 individuals (rows) and 5 variables (columns):
#' \describe{
#'  \item{Entertainment}{entertainment spending, 3 categories...Low, Mid, High (recoded from a 5-point Likert item, 1-2/3/4-5).}
#'  \item{Shopping}{shopping centres spending, 3 categories...Low, Mid, High (recoded from a 5-point Likert item, 1-2/3/4-5).}
#'  \item{Looks}{spending on looks, 3 categories...Low, Mid, High (recoded from a 5-point Likert item, 1-2/3/4-5).}
#'  \item{Gender}{gender, 2 categories...male, female.}
#'  \item{VillageTown}{village-town, 2 categories...village (living in a village), city (living in a city or large town).}
#' }
#' @source Young People Survey dataset (CC0: Public Domain license),
#' collected in 2013 from young Slovakians (aged 15-30) by students of
#' the Statistics class at FSEV UK (Faculty of Social and Economic
#' Sciences, Comenius University in Bratislava), N = 993 after removing
#' missing values on the variables used here.
#' \url{https://www.kaggle.com/datasets/miroslavsabo/young-people-survey}
"young"