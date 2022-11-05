#' Summarize Data
#'
#' @description This function will look at a data set and the names of variables of the
#'    data to conduct some summary output and helpful plots to better examine the data.
#'    This is helpful to see if there are any issues with a linear model for this specific
#'    data. Note that a red line signifies the base level on the plot.
#'
#' @param Y The name of the variable to be predicted
#' @param X Name of the quantitative independent variable.
#' @param XC Name of the qualitative dependent variable.
#' @param data Data set to be used for the analysis.
#'
#' @importFrom stats AIC anova coef confint influence.measures lm model.matrix resid
#' @importFrom ggplot2 ggplot aes geom_point labs ggtitle geom_boxplot geom_abline annotate
#'
#' @return This function will return 2 plots (in one window). There is a plot of
#'    The data with the regression lines added and a bar plot of each group of the
#'    categorical data. The following are also returned in a named list:
#'    \itemize{
#'       \item{betas}{Betas produced from a linear model of the data}
#'       \item{CI}{Confidence intervals for beta estimates of a model}
#'       \item{lmsum}{Summary output of a model used for analysis}
#'       \item{datasum}{A summary of the entire data set}
#'       \item{groupSum}{A summary of the data in each group}
#'    }
#' @export
#'
#' @examples
#' print(DataSummary("CH4", "TIME", "VHA-Added", SLUDGE))
DataSummary <- function(Y, X, XC, data){
  ##################### SETUP ########################

  data <- as.data.frame(data)

  Ydat <- as.vector(data[[Y]]) #Data for y
  Xdat <- as.vector(data[[X]]) #Quantitative data
  XCdat <- as.vector(data[[XC]]) #Qualitative Data

  max_x = Xdat[which.max(Xdat)] #Largest quantitative data

  factors <- unique(unlist(XCdat))
  factors = (factors[order(factors)]) #Ordered vector of factors


  ##################### Calculations ########################
  ylm <- lm(Ydat ~ Xdat + XCdat)


  ## Summary of each group
  GroupSum <- vector(mode = "list", length = length(factors))
  for(i in 1:length(factors)){
    tempdata <- Ydat[which(XCdat == factors[i])]
    GroupSum[[i]] = summary(tempdata)
  }


  ##################### Plotting ########################
  #Data plot
  g1 <- ggplot(data, aes(x = Xdat, y = Ydat)) +
    geom_point(aes(size = 1.5, colour = factor(XCdat))) +
    labs(x = X, y = Y) + ggtitle("Plot of Data")


  # Box Plots
  g2 <- ggplot(data, aes(x = XCdat, y = Ydat, fill = XCdat)) +
    geom_boxplot(aes(group = XCdat)) +
    geom_point() + labs(x = XC, y = Y, fill = XC) +
    ggtitle("Barplots of Groups")

  #Adding regression lines
  betas <- coef(ylm)
  for(i in 3:length(betas)){
    g1 = g1 + geom_abline(intercept = betas[1] + betas[i], slope = betas[2]) +
      annotate(geom="text", label=factors[i-1], x=max_x, y=betas[1] + betas[i] + betas[2]*max_x, vjust=1)
  }
  g1 = g1 + geom_abline(intercept = betas[1], slope = betas[2], col = "red") +
    annotate(geom="text", label=factors[1], x=max_x, y=betas[1] + betas[2]*max_x, vjust=0)

  print(g1 + g2)

  ## Return
  my_list <- list(betas = betas, CI = confint(ylm),
                  lmsum = summary(ylm), datasum = summary(data), groupSum = GroupSum)
  return(invisible(my_list))
}
