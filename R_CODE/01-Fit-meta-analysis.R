#######################################
## Fit 2-step meta analysis for all models: 3 tasks, 5 tasks, 8 tasks, and all 8-task-but-leave-one-out combinations
#######################################


inFile  <- "CONFIGURATION/conf_AllStudies8Tasks_new_leaveOneOut_UnscrambleWords1.R"
outFile <- "DATA_FITTED_MODELS/model-rem-diag-allS-8m_new_ExclUnscrambleWords1.Rdata"
source("R_CODE/helper_fitMetaAnalysis.R")

inFile  <- "CONFIGURATION/conf_AllStudies8Tasks_new_leaveOneOut_BrainstormObject1.R"
outFile <- "DATA_FITTED_MODELS/model-rem-diag-allS-8m_new_ExclBrainstormObject1.Rdata"
source("R_CODE/helper_fitMetaAnalysis.R")

inFile  <- "CONFIGURATION/conf_AllStudies8Tasks_new_leaveOneOut_TypingText1.R"
outFile <- "DATA_FITTED_MODELS/model-rem-diag-allS-8m_new_ExclTypingText1.Rdata"
source("R_CODE/helper_fitMetaAnalysis.R")

inFile  <- "CONFIGURATION/conf_AllStudies8Tasks_new_leaveOneOut_Sudoku1.R"
outFile <- "DATA_FITTED_MODELS/model-rem-diag-allS-8m_new_ExclSudoku1.Rdata"
source("R_CODE/helper_fitMetaAnalysis.R")

inFile  <- "CONFIGURATION/conf_AllStudies8Tasks_new_leaveOneOut_BrainstormWords1.R"
outFile <- "DATA_FITTED_MODELS/model-rem-diag-allS-8m_new_ExclBrainstormWords1.Rdata"
source("R_CODE/helper_fitMetaAnalysis.R")

inFile  <- "CONFIGURATION/conf_AllStudies8Tasks_new_leaveOneOut_MemoryPicture1.R"
outFile <- "DATA_FITTED_MODELS/model-rem-diag-allS-8m_new_ExclMemoryPicture1.Rdata"
source("R_CODE/helper_fitMetaAnalysis.R")

inFile  <- "CONFIGURATION/conf_AllStudies8Tasks_new_leaveOneOut_TypingNumbers1.R"
outFile <- "DATA_FITTED_MODELS/model-rem-diag-allS-8m_new_ExclTypingNumbers1.Rdata"
source("R_CODE/helper_fitMetaAnalysis.R")

inFile  <- "CONFIGURATION/conf_AllStudies8Tasks_new_leaveOneOut_MatrixSolving1.R"
outFile <- "DATA_FITTED_MODELS/model-rem-diag-allS-8m_new_ExclMatrixSolving1.Rdata"
source("R_CODE/helper_fitMetaAnalysis.R")

inFile  <- "CONFIGURATION/conf_AllStudies3Tasks_new.R"
outFile <- "DATA_FITTED_MODELS/model-rem-diag-allS-3m.Rdata"
source("R_CODE/helper_fitMetaAnalysis.R")

inFile  <- "CONFIGURATION/conf_AllStudies5Tasks_new.R"
outFile <- "DATA_FITTED_MODELS/model-rem-diag-allS-5m.Rdata"
source("R_CODE/helper_fitMetaAnalysis.R")

inFile  <- "CONFIGURATION/conf_AllStudies7Tasks_new.R"
outFile <- "DATA_FITTED_MODELS/model-rem-diag-allS-7m.Rdata"
source("R_CODE/helper_fitMetaAnalysis.R")

## Make sure to run the full 8-task model ("prefered model") last 
## We use this model (the "desc" object) for (a) pooled correlations in Figure 2A and Table S3; and (b) summary of samples in Table S1
inFile  <- "CONFIGURATION/conf_AllStudies8Tasks_new.R"
outFile <- "DATA_FITTED_MODELS/model-rem-diag-allS-8m.Rdata"
source("R_CODE/helper_fitMetaAnalysis.R")


