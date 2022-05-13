###
# Latent Profile Analysis 
# for the confirmatory study
###

library('tidyLPA')
library('ggplot2')

data <- read.csv('averages_with_indiv_measures.csv')
data <- subset(data, study=='main')

#model comparison
profiles <- estimate_profiles(df = data[2:3], n_profiles = 1:4, variances = "equal",
                  covariances = "zero") %>% compare_solutions(statistics = c("AIC", "BIC"))

profiles

#fitting a model with 3 classes
by_partic.est_classes <- estimate_profiles(data[2:3], n_profiles = 3, variances="equal",covariances="zero") %>% 
  get_data()
by_partic.est_classes <- data.frame(by_partic.est_classes)
by_partic.est_classes$id  <- 1:nrow(by_partic.est_classes)
data$id <- 1:nrow(data)

data$Class <- as.factor(by_partic.est_classes$Class[match(by_partic.est_classes$id,data$id)])

#Figure 4 in the paper
lpaplot <- ggplot(data=data, aes(x=simple ,y=complex, shape = Class, color=Class)) + geom_count() + xlim(0.0,1.1) +ylim(0,1)+
  xlab('simple: prop correct') + ylab('complex: prop correct') + theme(text = element_text(size = 25)) + scale_size_area(max_size = 14) 

lpaplot + guides(color = guide_legend(override.aes = list(size = 5)))
