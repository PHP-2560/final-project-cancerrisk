library(ggplot2)
lung <- read.csv("lung.csv")
lung

ggplot(lung, aes(x=Smokerate, y=Cancerrate, color=Year)) +
  geom_point(size=10) +
  xlab("Smoking Rate") +
  ylab("Cancer Rate") +
  ggtitle("Smoking is Directly Correlated to Lung Cancer") +
  facet_grid(~Region)+
  geom_smooth(method = "lm") +
  theme_classic()
