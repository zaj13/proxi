#' Dependent t-test
#'
#' This function calculates a dependent t-test and relevant supplementary statistics.
#' @param data The data frame to evaluate.
#' @param dependent.var1 First measurement variable of the data frame.
#' @param dependent.var2 Second measurement variable of the data frame.
#' @importFrom nortest ad.test
#' @importFrom lsr cohensD
#' @keywords dependent t-test
#' @export
#' @examples
#' carsp <- data.frame(speed1 = cars$speed[1:(nrow(cars)/2)], speed2 = cars$speed[(nrow(cars)/2+1):nrow(cars)])
#' results.carsp <- proxi::dependentt(carsp, speed1, speed2)

dependentt <- function(data, dependent.var1, dependent.var2){
  
  eClass <- "Argument 'data' is not of class data.frame"
  
  # check class
  if (class(data) != "data.frame") {
    stop(eClass)
  }
  
  dependent.var2 <- eval(substitute(dependent.var2), data, parent.frame())
  dependent.var1 <- eval(substitute(dependent.var1), data, parent.frame())
  results <- -1
  
  qqnorm(dependent.var2 - dependent.var1)
  qqline(dependent.var2 - dependent.var1)
  
  results <- list(normShapiro = shapiro.test(dependent.var2 - dependent.var1),
                  normAnderson = ad.test(dependent.var2 - dependent.var1),
                  corrPearson = cor.test(x = dependent.var1, y = dependent.var2),
                  ttest = t.test(dependent.var1, dependent.var2, paired = TRUE),
                  EScohensD = cohensD(dependent.var1, dependent.var2),
                  qqChart = recordPlot()
             )
  return(results)
}