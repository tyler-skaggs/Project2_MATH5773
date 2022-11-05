#' Analysis of Residuals
#'
#' @description This funciton looks at the model passed in and will conduct various
#'    reports about the data that can be helpful, specifically we are looking at potential
#'    issues with the residuals and if any specific points should be examined further and
#'    for which reason.
#'
#' @param model The model to be examined, of the form y ~ x1 + x2
#' @param data The data frame of which the data is pulled from
#'
#' @importFrom stats AIC anova coef confint influence.measures lm model.matrix resid
#' @importFrom ggplot2 ggplot aes geom_point geom_segment scale_color_continuous geom_hline
#'    theme_bw ggtitle geom_col
#' @importFrom ggplotify as.grob
#' @importFrom utils globalVariables
#' @import patchwork
#'
#' @return This function will return 2 plots (in one window). There is a plot of
#'    residuals vs predicted, and a plot of cooks distance. The
#'    following are also returned in a named list:
#'    \itemize{
#'       \item{InfMeasures}{A dataframe that contains the indicies and reasons for which
#'       data values could be considered as having high influence}
#'       \item{anova}{A summary output from an anova test on the model}
#'       \item{resids}{A vector of residuals from the model}
#'       \item{P}{The Hat matrix for the model}
#'       \item{r.sqd}{The calculated \eqn{r^2} value of the model, supplied by \code{lm}}
#'       \item{AIC}{The calculated AIC value of the model}
#'    }
#' @export
#'
#' @examples
#' print(ResAn(TIME ~ NUMBER + BROWSER, BROWSER))
ResAn <- function(model, data){
  data = data.frame(data)
  ylm <- lm(model, data)

  X <- model.matrix(ylm)
  P <- X %*% solve(t(X) %*% X) %*% t(X)

  residuals <- resid(ylm)

  #Plot of Residuals
  g1 <- ggplot(data.frame(Index = 1:length(residuals), residuals), aes(x = Index, y = residuals)) +
    geom_point(aes(color = abs(residuals), size = abs(residuals))) +
    geom_segment(aes(xend = Index, yend = 0)) +
    scale_color_continuous(low = "green", high = "red") +
    geom_hline(yintercept = 0) +
    theme_bw() + ggtitle("Residuals")

  #Cook's Distance
  g2 <- ggplot(ylm, aes(seq_along(.cooksd), .cooksd)) +
    geom_col() + ggtitle("Cook's Distance")

  print(g1 + g2)

  # Looking at influence measures
  infl <- influence.measures(ylm)
  ## Index of which item it is (row)
  Index <- which(infl$is.inf == TRUE) %% dim(infl$is.inf)[1]
  ##Finding the type of influence caught (column)
  InfType <- colnames(as.data.frame(infl$is.inf))[ceiling(which(infl$is.inf == TRUE)/dim(infl$is.inf)[1])]

  my_list <- list(InfMeasures = data.frame(Index, InfType)[order(Index, decreasing = FALSE), ],
                  anova = anova(ylm), resids = residuals, Fval = summary(ylm)$fstatistic,
                  P = P, r.sqd = summary(ylm)$adj.r.squared, AIC = AIC(ylm))

  return(invisible(my_list))
}
