#' Independent t-test
#'
#' This function calculates an independent t-test and relevant supplementary statistics.
#' @param data The data frame to evaluate.
#' @param dependent.var Dependent variable of the data frame.
#' @param group.var Grouping variable of the data frame. Has to equal two.
#' @importFrom car leveneTest
#' @importFrom nortest ad.test
#' @importFrom lsr cohensD
#' @keywords independent t-test
#' @export
#' @examples
#' carsp <- data.frame(speed = cars$speed, group = as.factor(sample(1:2,nrow(cars),replace = TRUE)))
#' results.carsp <- proxi::independentt(carsp, speed, group)

independentt <- function(data, dependent.var, group.var){
  
  eClass <- "Argument 'data' is not of class data.frame"
  eGroupLevels <- "Number of groups does not equal two."

  # check class  
  if (class(data) != "data.frame") {
    stop(eClass)
  }
  
  group.var <- eval(substitute(group.var), data, parent.frame())
  dependent.var <- eval(substitute(dependent.var), data, parent.frame())
  results <- -1

  #check number of factors and calculate results
  if (length(levels(as.factor(group.var))) != 2) {
    stop(eGroupLevels)
  }else results <- list(normShapiro = shapiro.test(dependent.var),
                        normAnderson = ad.test(dependent.var),
                        hovBartlett = bartlett.test(dependent.var ~ as.factor(group.var)),
                        hovLevene = leveneTest(dependent.var ~ as.factor(group.var)),
                        ttest.equal = t.test(dependent.var ~ as.factor(group.var), var.equal = TRUE),
                        ttest.unequal = t.test(dependent.var ~ as.factor(group.var)),
                        EScohensD = cohensD(dependent.var ~ group.var)
                   )
  return(results)
}