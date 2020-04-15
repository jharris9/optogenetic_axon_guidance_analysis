require('dplyr')
#require('lme4')
require('lmertest')
#require('car')
require('gmodels')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./r_utils.r")
#see https://cran.r-project.org/web/packages/lmertest//lmertest.pdf
zfish_data_init = read.csv("./200217_plod3_quantification.csv")
x = zfish_data %>% group_by(Sample) %>% group_by(Neuron) %>% summarise(mean_dist = mean(Distance_below_hms), ci = ci(Distance_below_hms)[3]-ci(Distance_below_hms)[1] )
#zfish_data$date = as.character(zfish_data$date)
#here formula describes distance to somite as a function of a fixed
#effect of location (independent) and a random effect of date (interdependent)
zfish_data = zfish_data %>% filter(!grepl("190109_zfish_plod3_rescue",Sample))
model = lmer(Distance_below_hms ~ Neuron + (1+Neuron|Sample), data=zfish_data, REML = FALSE, 
             control = lmerControl(optimizer ="Nelder_Mead"))
#null = lmer(distance_to_somite ~ 1 + (1+location|date),data=zfish_data, REML=FALSE)
summary(model)
coef(model)
#confint(model)
#anova(null,model)

(aov <-anova(model))
show_tests(aov, fractions = TRUE)$Neuron
if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(model, type=2, ddf="Kenward-Roger")
ranova(model)
drop1(model)
(lsm <- ls_means(model))
ls_means_table = ls_means(model, which="Neuron", pairwise = TRUE)
ls_means_table2 = data.frame(names = row.names(ls_means_table),padj=p.adjust(ls_means_table$`Pr(>|t|)`,method="fdr"), original_pval=ls_means_table$`Pr(>|t|)`)
ls_means(model, which = "Neuron", pairwise = TRUE)
plot(lsm, which=c("Neuron"))
as.data.frame(lsm)
show_tests(lsm, fractions=TRUE)$location
(L <- diag(length(fixef(model)))[2:3, ])
contest(model, L = L)
step_result <- step(model)
step_result
final_model <- get_model(step_result)
final_model

zfish_data$Neuron = factor(zfish_data$Neuron,levels=c("plod3","control","rescued"))
plot = ggDotPlot(zfish_data,"Neuron","Distance_below_hms",c("black","black","red"))
plot = plot + ylab("Distance to HMS (um)") + geom_hline(yintercept = 0, size=2, linetype=2)
plot 
plot_size=8#getPlotSize()
ggsave(
  file = paste0(
    format(Sys.Date(), "%y%m%d"),
    "_zfish_plod3_quantification.pdf"
  ),
  path = "./",
  plot = last_plot(),
  width = plot_size,
  height = plot_size,
  units = "in",
  dpi = 300
)

