## plots for (parts of) figure 1

# library
library(ggplot2)
library(ggExtra)
library(gridExtra)

# Make 3 simple graphics:
g2 <- ggplot(mtcars, aes(x=drat, y=qsec, color=factor(cyl))) +
  geom_point(size=5) + theme(legend.position="none",
                             axis.text.x=element_blank(),
                             axis.text.y=element_blank()) + 
  scale_colour_grey() + labs(x = "", y = "")
g3 <- ggplot(mtcars, aes(x=factor(cyl), y=qsec, fill=factor(cyl))) + 
  geom_boxplot() + theme(legend.position="none",
                         axis.text.x=element_blank(),
                         axis.text.y=element_blank()) +
  labs(x = "", y = "") + scale_fill_grey()
g4 <- ggplot(mtcars , aes(x=factor(cyl), fill=factor(cyl))) +  geom_bar() +
  theme(legend.position="none",
  axis.text.x=element_blank(),
  axis.text.y=element_blank()) + 
  labs(x = "", y = "") + scale_fill_grey()

# Plots
p_all <- grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow = 2)

ggsave(plot = p_all, filename = "Fig1_part.pdf", units = "cm",
       width = 6, height = 10, dpi = 300, scale = 1)



p2 <- ggplot(mtcars, aes(x=mpg, y=wt, color=factor(gear))) +
  geom_point(size=5) + theme(legend.position="none",
                             axis.text.x=element_blank(),
                             axis.text.y=element_blank()) + 
  scale_colour_grey() + labs(x = "", y = "")
p3 <- ggplot(mtcars, aes(x=factor(gear), y=wt, fill=factor(gear))) + 
  geom_boxplot() + theme(legend.position="none",
                         axis.text.x=element_blank(),
                         axis.text.y=element_blank()) +
  labs(x = "", y = "") + scale_fill_grey()
p4 <- ggplot(mtcars , aes(x=factor(gear), fill=factor(gear))) +  geom_bar() +
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) + 
  labs(x = "", y = "") + scale_fill_grey()

g_all <- grid.arrange(p2, arrangeGrob(p3, p4, ncol=2), nrow = 2)

ggsave(plot = g_all, filename = "Fig1_part2.pdf", units = "cm",
       width = 6, height = 10, dpi = 300, scale = 1)
