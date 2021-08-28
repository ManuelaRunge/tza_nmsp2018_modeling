## -----------------------------------------
#### Figure 2 - explored strategies
## -----------------------------------------

library(tidyverse)
library(data.table)
library(spatstat)
library(cowplot)
prevcolsAdj <- c("darkorchid2", "#1A9850", "#91CF60", "gold2", "#e31a1c")
theme_set(theme_cowplot())

require(devtools)
source_gist("9d50c86e21161188a6c17c7c9650ad5d", filename = "aggrDat.R")

load(file.path("simdat", "AnalysisDat.RData"))
baselineDat <- AnalysisDat %>%
  filter(year == 2016) %>%
  f_weighted.aggrDat("StrataLabel", "PR", "Population_2016", WideToLong = FALSE)

baselineDat$StrataLabel2 <- factor(baselineDat$StrataLabel,
                                   levels = c("very low", "low", "urban", "moderate", "high"),
                                   labels = c("very low (6)", "low (5)", "urban (9)", "moderate (6)", "high (11)")
)

dat <- AnalysisDat %>%
  filter(year == 2016) %>%
  dplyr::select(FutScen, futSNPcov, FutScen_nr) %>%
  unique()

ScenDat <- fread(file.path("simdat", "Figure2_strata.csv"))
dim(ScenDat)

ScenDat <- left_join(ScenDat, dat) %>% as.data.frame()
table(ScenDat$FutScen_label_noCM, exclude = NULL)

ScenDat$FutScen_label_noCM <- gsub("+continuous", "SNP", ScenDat$FutScen_label_noCM)
ScenDat$FutScen_label_noCM <- gsub("ITN continuous", "ITN(SNP)", ScenDat$FutScen_label_noCM)
ScenDat$FutScen_label_noCM <- gsub("ITN MRC", "ITN(MRC)", ScenDat$FutScen_label_noCM)
ScenDat$FutScen_label_noCM <- gsub(" ", "", ScenDat$FutScen_label_noCM)
ScenDat$FutScen_label_noCM <- gsub("ITNSNP", "ITN(SNP)", ScenDat$FutScen_label_noCM)
ScenDat$futSNPcov[ScenDat$futSNPcov == 0] <- NA
ScenDat$futSNPcov <- ScenDat$futSNPcov * 100
ScenDat$FutScen_label_noCM <- paste0(ScenDat$FutScen_label_noCM, "-s", ScenDat$futSNPcov)

verylowFinal <- 77 # revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM
lowFinal <- 125 # revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM
urbanFinal <- c(73, 85, 133, 134) # revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM
moderateFinal <- 137 #revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM
highFinal <- c(102, 108) #revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM


ScenDat$StrataLabel <- factor(ScenDat$StrataLabel,
                              levels = c("very low", "moderate", "low", "high", "urban"),
                              labels = c("very low", "moderate", "low", "high", "urban"))

baselineDat$StrataLabel <- factor(baselineDat$StrataLabel,
                                  levels = c("very low", "moderate", "low", "high", "urban"),
                                  labels = c("very low", "moderate", "low", "high", "urban"))


#### PLOT
pplot <- ggplot(data = baselineDat) +
  theme_cowplot() +
  geom_pointrange(data = ScenDat, aes(x = as.factor(FutScen_label_noCM),
                                      y = mean.val,
                                      ymin = lower.ci.val,
                                      ymax = upper.ci.val, alpha = CMincrease)) +
  geom_pointrange(data = subset(ScenDat, StrataLabel == "very low" & FutScen_nr %in% verylowFinal),
                  aes(x = FutScen_label_noCM,
                      y = mean.val,
                      ymin = lower.ci.val,
                      ymax = upper.ci.val),
                  col = prevcolsAdj[2]) +
  geom_pointrange(data = subset(ScenDat, StrataLabel == "low" & FutScen_nr %in% lowFinal),
                  aes(x = FutScen_label_noCM,
                      y = mean.val,
                      ymin = lower.ci.val,
                      ymax = upper.ci.val), col = prevcolsAdj[3]) +
  geom_pointrange(data = subset(ScenDat, StrataLabel == "urban" & FutScen_nr %in% urbanFinal),
                  aes(x = FutScen_label_noCM,
                      y = mean.val,
                      ymin = lower.ci.val,
                      ymax = upper.ci.val), col = prevcolsAdj[1]) +
  geom_pointrange(data = subset(ScenDat, StrataLabel == "moderate" & FutScen_nr %in% moderateFinal),
                  aes(x = FutScen_label_noCM,
                      y = mean.val,
                      ymin = lower.ci.val,
                      ymax = upper.ci.val), col = prevcolsAdj[4]) +
  geom_pointrange(data = subset(ScenDat, StrataLabel == "high" & FutScen_nr %in% highFinal),
                  aes(x = FutScen_label_noCM,
                      y = mean.val,
                      ymin = lower.ci.val,
                      ymax = upper.ci.val), col = prevcolsAdj[5]) +
  geom_hline(data = baselineDat, aes(yintercept = mean.val), linetype = "dashed") +
  geom_hline(yintercept = c(Inf)) +
  coord_flip() +
  theme(legend.position = "none") +
  geom_vline(xintercept = c(Inf, -Inf)) +
  labs(y = expression(italic("PfPR")["2 to 10"] * ""), x = "Intervention packages per strata (unique number)") +
  facet_wrap(~StrataLabel, scales = "free", ncol = 2) +
  theme(
    panel.spacing.x = unit(0, "line"),
    strip.text.x = element_text(size = 22, face = "bold"),
    strip.text.y = element_text(size = 22, face = "bold"),
    strip.placement = "outside",
    strip.background = element_rect(colour = "black", fill = "white"),
    plot.subtitle = element_text(hjust = -0.25, face = "bold", size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22)
  )

# p <- pplot
# g <- ggplot_gtable(ggplot_build(p))
# strip_both <- which(grepl("strip-", g$layout$name))
# fills <- prevcolsAdj[c(5, 4, 1, 3, 2)]
# k <- 1
# for (i in strip_both) {
#   j <- which(grepl("rect", g$grobs[[i]]$grobs[[1]]$childrenOrder))
#   g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
#   k <- k + 1
# }
# grid.draw(g)
# plotAadj <- g

ggsave("Fig_2.png", plot = pplot, path = 'figures', width = 14, height = 10, device = "png")
ggsave("Fig_2.pdf", plot = pplot, path = 'figures', width = 14, height = 10, device = "pdf")



### For text
fwrite(ScenDat, file.path("figures","figure2_dat.csv"))
fwrite(baselineDat, file.path("figures","baselineDat.csv"))