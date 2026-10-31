## figure 2 of the paper

load("data.RData")

#oral part
oralik <- 
  as.data.frame(apply(t(oral_scores), 2, factor,
                      simplify = F, levels = c("1","2","3","4","5")))

library(plyr)
library(ggplot2)

orh <- likert::likert.bar.plot(likert::likert(oralik[oral_meta$llm_or_human=="human",]),
                               low.color = "brown", high.color = "steelblue",
                               neutral.color = "grey",
                               ordered = F, legend = "Likert Score", plot.percent.high = F, 
                               plot.percent.low = F, plot.percents = T,
                               group.order = names(oralik)) + 
  ggtitle("Human Doctors") 
orh_legend <- ggpubr::get_legend(orh)
orh <- likert::likert.bar.plot(likert::likert(oralik[oral_meta$llm_or_human=="human",], nlevels = 5),
                               center = 3,
                               low.color = "brown", high.color = "steelblue",
                               neutral.color = "grey",
                               ordered = F, plot.percent.high = F, 
                               plot.percent.low = F, plot.percents = F,
                               group.order = names(oralik)) + 
  ggtitle("Human Doctors") +
  theme(legend.position = "none")
  
orl <- likert::likert.bar.plot(likert::likert(oralik[oral_meta$llm_or_human=="llm",], nlevels = 5),
                               center = 3,
                               low.color = "brown", high.color = "steelblue",
                               neutral.color = "grey",
                               ordered = F, plot.percent.high = F, 
                               plot.percent.low = F, plot.percents = F,
                               group.order = names(oralik)) +
  ggtitle("LLMs") +
  theme(legend.position = "none")

pan_A <-
  gridExtra::grid.arrange(orh, orl,
                          layout_matrix = matrix(c(1,NA,2), nrow = 1, byrow = T),
                          widths = c(3,1,3),
                          top = ggpubr::text_grob("A) Simulated Doctor Visit Scores",
                                                  hjust = 1.91, size = 16)
                          )

#patient part
patielik <- 
  as.data.frame(apply(t(patient_scores), 2, factor,
                      simplify = F, levels = c("1","2","3","4","5")))

path <- 
  likert::likert.bar.plot(likert::likert(patielik[patient_meta$llm_or_human=="human",], nlevels = 5),
                          center = 3,
                          low.color = "brown", high.color = "steelblue",
                          neutral.color = "grey",
                          ordered = F, plot.percent.high = F, 
                          plot.percent.low = F, plot.percents = F,
                          group.order = names(patielik)) + 
  ggtitle("Human Doctors") +
  theme(legend.position = "none")

patl <- 
  likert::likert.bar.plot(likert::likert(patielik[patient_meta$llm_or_human=="llm",], nlevels = 5),
                          center = 3,
                          low.color = "brown", high.color = "steelblue",
                          neutral.color = "grey",
                          ordered = F, plot.percent.high = F, 
                          plot.percent.low = F, plot.percents = F,
                          group.order = names(patielik)) +
  ggtitle("LLMs") +
  theme(legend.position = "none")

pan_B <-
  gridExtra::grid.arrange(path, patl,
                          layout_matrix = matrix(c(1,NA,2), nrow = 1, byrow = T),
                          widths = c(3,1,3),
                          top = ggpubr::text_grob("B) Simulated Patient Experience Scores",
                                                  hjust = 1.56, size = 16)
  )


#written part
writlik <- 
  as.data.frame(apply(t(written_scores), 2, factor,
                      simplify = F, levels = c("1","2","3","4","5")))

wrih <- 
  likert::likert.bar.plot(likert::likert(writlik[written_meta$llm_or_human=="human",], nlevels = 5),
                          center = 3,
                          low.color = "brown", high.color = "steelblue",
                          neutral.color = "grey",
                          ordered = F, plot.percent.high = F, 
                          plot.percent.low = F, plot.percents = F,
                          group.order = names(writlik)) + 
  ggtitle("Human Doctors") +
  theme(legend.position = "none")

wril <- 
  likert::likert.bar.plot(likert::likert(writlik[written_meta$llm_or_human=="llm",], nlevels = 5),
                          center = 3,
                          low.color = "brown", high.color = "steelblue",
                          neutral.color = "grey",
                          ordered = F, plot.percent.high = F, 
                          plot.percent.low = F, plot.percents = F,
                          group.order = names(writlik)) +
  ggtitle("LLMs") +
  theme(legend.position = "none")

pan_C <-
  gridExtra::grid.arrange(wrih, wril,
                          layout_matrix = matrix(c(1,NA,2), nrow = 1, byrow = T),
                          widths = c(3,1,3),
                          top = ggpubr::text_grob("C) Written Exam Scores",
                                                  hjust = 2.58, size = 16)
  )

fig2 <- 
  gridExtra::grid.arrange(pan_A, pan_B, pan_C, ggpubr::as_ggplot(orh_legend),
                          heights = c(5,8,7,1),
                          ncol = 1,
                          top = ggpubr::text_grob("Likert Scores of Human Doctors Vs. LLMs", size = 18)
  )

ggsave(plot = fig2, filename = "Fig2.pdf", units = "cm", width = 21, height = 29.7, dpi = 300, scale = 1.5)
ggsave(plot = fig2, filename = "Fig2.jpg", units = "cm", width = 21, height = 29.7, dpi = 300, scale = 1.5)


# p values of wilcoxon for each horizontal bar comparison
oral_pvals <- 
  apply(oral_scores, 1,
        \(x) wilcox.test(x[oral_meta$llm_or_human == "human"],
                         x[oral_meta$llm_or_human == "llm"],
                         alternative = "less")$p.value)
format(oral_pvals, scientific = F)
patient_pvals <- 
  apply(patient_scores, 1,
        \(x) wilcox.test(x[patient_meta$llm_or_human == "human"],
                         x[patient_meta$llm_or_human == "llm"],
                         alternative = "less")$p.value)
format(patient_pvals, scientific = F)
written_pvals <- 
  apply(written_scores, 1,
        \(x) wilcox.test(x[written_meta$llm_or_human == "human"],
                         x[written_meta$llm_or_human == "llm"],
                         alternative = "less")$p.value)
format(written_pvals, scientific = F)
