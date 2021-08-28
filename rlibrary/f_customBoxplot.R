## To Do - write mean cost over boxplot	
f_customBoxplot <- function(tempdat, yvar) {
  # tempdat 	= subset(OMStrategyDat,year==2020)
  # yvar 		= "PR"

  titlelabel <- EvaluationYears[length(EvaluationYears)]
  yvarLabel <- yvar
  if (yvar == "PR") yvarLabel <- expression(italic("Pf") * "PR"["2 to 10"] * "(%)")
  if (sum(grep("cum", yvar)) > 0) titlelabel <- paste0(baselineYear, "-", EvaluationYears[length(EvaluationYears)])

  tempdat$tempvar <- tempdat[, colnames(tempdat) == yvar]
  if (yvar == "PR") tempdat$tempvar <- tempdat$tempvar * 100

  tempplot <- ggplot(data = tempdat) +
    geom_boxplot(aes(x = reorder(Strategy_adj, tempvar), y = tempvar), fill = "cornflowerblue", width = 0.5) +
    labs(title = titlelabel, subtitle = "", y = yvarLabel, x = "") +
    scale_y_continuous(label = comma, expand = c(0, 0)) +
    geom_hline(yintercept = 0, col = "black", size = 0.7) +
    customTheme
  return(tempplot)
}