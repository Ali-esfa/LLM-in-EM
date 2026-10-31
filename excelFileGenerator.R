## making an .xlsx file from data.RData
#by ali.esfa
load("data.RData")

openxlsx::write.xlsx(list("Doctor_Visit_Meta" = oral_meta[,-1],
                          "Doctor_Visit_Scores" = oral_scores,
                          "Patient_Experience_Meta" = patient_meta[,-1],
                          "Patient_Experience_Scores" = patient_scores,
                          "Written_exam_Meta" = written_meta[,-1],
                          "Written_exam_Scores" = written_scores),
                     file = "data.xlsx",
                     rowNames = TRUE, widths = "auto")
