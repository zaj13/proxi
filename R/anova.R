#' Analysis of Variance
#'
#' This function calculates a One-Way ANOVA and relevant supplementary statistics.
#' @param data The data frame to evaluate.
#' @param dependent.var Dependent variable of the data frame.
#' @param group.var Grouping variable of the data frame. Can be greater 2.
#' @importFrom car leveneTest
#' @importFrom nortest ad.test
#' @importFrom lsr etaSquared
#' @keywords ANOVA variance analysis
#' @export
#' @examples
#' carsp <- data.frame(speed = cars$speed, group = as.factor(sample(1:3,nrow(cars),replace = TRUE)))
#' results.carsp <- proxi::independentt(carsp, speed, group)

anova <- function(data, dependent.var, group.var){
  
  eClass <- "Argument 'data' is not of class data.frame"
  eGroupLevels <- "Number of groups needs to be >1"
  
  if (class(data) != "data.frame") {
    stop(eClass)
  }
  
  group.var <- eval(substitute(group.var), data, parent.frame())
  dependent.var <- eval(substitute(dependent.var), data, parent.frame())
  results <- -1
    
  #check number of factors and calculate results
  if (!length(levels(as.factor(group.var))) >= 2) {
    stop(eGroupLevels)
  }else {
    qqnorm(dependent.var)
    qqline(dependent.var)
    results <- list(normShapiro = shapiro.test(dependent.var),
                        normAnderson = ad.test(dependent.var),
                        hovBartlett = bartlett.test(dependent.var ~ as.factor(group.var)),
                        hovLevene = leveneTest(dependent.var ~ as.factor(group.var)),
                        ANOVA = summary.aov(aov(dependent.var ~ group.var)),
                        ESeta2 = etaSquared(aov(dependent.var ~ group.var),type = 3),
                        qqChart = recordPlot()
               )
  }
  return(results)

}