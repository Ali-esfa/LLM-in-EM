# script making figure 3 of the paper
# the forest plots

library(dplyr)
library(ggplot2)
load("data.RData")

levelordered <- c("chatgpt_5.6_sol",
                  "deepseek_r1",
                  "deepseek_v4_pro",
                  "gemini_3.1_pro",
                  "o3",
                  "qwen_3.7_max_fast",
                  "qwen_3.7_max_thinking",
                  "claude_4.8_opus")

patient_factor_model <- 
  psych::fa(t(patient_scores), nfactor=1, cor="poly",
            rotate = "none", scores = "regression")

patient_factor_scores <- c(patient_factor_model$scores)

patient_llm_lm <-
  lm(cbind(patient_factor_scores, patient_meta) %>%
       filter(llm_or_human == "llm") %>%
       mutate(llmorhuman_detail = factor(llmorhuman_detail, levels = levelordered)),
     formula = patient_factor_scores ~ llmorhuman_detail)


oral_factor_model <- 
  psych::fa(t(oral_scores), nfactor=1, cor="poly",
            rotate = "none", scores = "regression")

oral_factor_scores <- c(oral_factor_model$scores)

oral_llm_lm <-
  lm(cbind(oral_factor_scores, oral_meta) %>%
       filter(llm_or_human == "llm") %>%
       mutate(llmorhuman_detail = factor(llmorhuman_detail, levels = levelordered)),
     formula = oral_factor_scores ~ llmorhuman_detail)


# written_factor_model <- 
#   psych::fa(t(written_scores), nfactor=1, cor="poly",
#             rotate = "none", scores = "regression")
# 
# written_factor_scores <- c(written_factor_model$scores)

diags <- apply(written_scores[1:2,], 2, mean)
written_llm_lm_diag <-
  lm(cbind(diags, written_meta) %>%
       filter(llm_or_human == "llm") %>%
       mutate(llmorhuman_detail = factor(llmorhuman_detail, levels = levelordered)),
     formula = diags ~ llmorhuman_detail)

inve <- apply(written_scores[3:4,], 2, mean)
written_llm_lm_inve <-
  lm(cbind(inve, written_meta) %>%
       filter(llm_or_human == "llm") %>%
       mutate(llmorhuman_detail = factor(llmorhuman_detail, levels = levelordered)),
     formula = inve ~ llmorhuman_detail)

trea <- apply(written_scores[5:6,], 2, mean)
written_llm_lm_trea <-
  lm(cbind(trea, written_meta) %>%
       filter(llm_or_human == "llm") %>%
       mutate(llmorhuman_detail = factor(llmorhuman_detail, levels = levelordered)),
     formula = trea ~ llmorhuman_detail)


#--- ggforestplotR

LLMBeautifier2 <- function(llm){
  stopifnot(all(llm %in% c("llmorhuman_detailchatgpt_5.6_sol",
                           "llmorhuman_detailclaude_4.8_opus",
                           "llmorhuman_detaildeepseek_r1",
                           "llmorhuman_detaildeepseek_v4_pro",
                           "llmorhuman_detailgemini_3.1_pro",
                           "llmorhuman_detailo3",
                           "llmorhuman_detailqwen_3.7_max_fast",
                           "llmorhuman_detailqwen_3.7_max_thinking")))
  mapper <- c("ChatGPT 5.6 Sol",
              "Claude 4.8 Opus",
              "DeepSeek R1",
              "DeepSeek V4 Pro",
              "Gemini 3.1 Pro",
              "O3",
              "Qwen 3.7 Max (Fast)",
              "Qwen 3.7 Max (Thinking)")
  names(mapper) <- c("llmorhuman_detailchatgpt_5.6_sol",
                     "llmorhuman_detailclaude_4.8_opus",
                     "llmorhuman_detaildeepseek_r1",
                     "llmorhuman_detaildeepseek_v4_pro",
                     "llmorhuman_detailgemini_3.1_pro",
                     "llmorhuman_detailo3",
                     "llmorhuman_detailqwen_3.7_max_fast",
                     "llmorhuman_detailqwen_3.7_max_thinking")
  
  mapper[llm]
}

column_label_beautifier <- c("LLM", "Score Difference (95% Cl)", "p value")
names(column_label_beautifier) <- c("Term", "estimate", "p.value")


bound_models <- ggforestplotR::bind_forest_models(list(oral_llm_lm,
                                        patient_llm_lm,
                                        written_llm_lm_diag,
                                        written_llm_lm_inve,
                                        written_llm_lm_trea),
                                   model_labels = c("Simulated Doctor Visit", 
                                                    "Simulated Patient Experience",
                                                    "Written Exam (Diagnosis)",
                                                    "Written Exam (Investigation)",
                                                    "Written Exam (Treatment)"))

bound_models$label <- LLMBeautifier2(bound_models$label)
bound_models$group <-
  factor(bound_models$group,
  levels = c("Simulated Doctor Visit",
            "Simulated Patient Experience",
            "Written Exam (Diagnosis)",
            "Written Exam (Investigation)",
            "Written Exam (Treatment)")[5:1])

fig4neu <- ggforestplotR::ggforestplot(bound_models, striped_rows = T, p.value = "p.value") +
  # scale_x_continuous(limits = c(-6,1)) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0)) +
  scale_color_manual(values = c("firebrick4", "firebrick3", "firebrick1",
                                "#1F968BFF", "#453781FF"),
                     name = "") +
  ggforestplotR::add_forest_table(columns = c("term", "group", "estimate", "p.value"),
                   column_labels = column_label_beautifier,
                   p_digits = 3,
  ) + labs(x = "Score Difference (95% Cl)", title = "LLM Performance Differences Quantified")

ggsave(plot = fig4neu, filename = "Fig4neu.pdf", units = "cm",
       width = 21, height = 15, dpi = 300, scale = 1.6)

# ggsave(plot = fig4neu, filename = "Fig4neu.png", units = "cm",
#        width = 21, height = 10, dpi = 300, scale = 1)
