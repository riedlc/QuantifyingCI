#######################################
## Create tables and figures
#######################################


## Configuration
tasks       <- c("UnscrambleWords1", "BrainstormObject1", "TypingText1", "Sudoku1", "BrainstormWords1", "MemoryPicture1", "TypingNumbers1", "MatrixSolving1")
tasksPretty <- c("Unscramble Words", "Brainstorm Object", "Typing Text", "Sudoku", "Brainstorm Words", "Memory Picture", "Typing Numbers", "Matrix Reasoning"); names(tasksPretty) <- tasks


# Load primary team-level data file
m <- read.csv( mainDataFile, stringsAsFactors=FALSE)
m$GroupSizeSq <- m$GroupSize^2
m$AgeDiversity <- factor( m$AgeDiversity, levels = c("Low", "Medium", "High"))

# Check if script to fit all models has been run yet
if( ! file.exists("DATA_FITTED_MODELS/model-rem-diag-allS-8m.Rdata") ) {
  stop( "You first need to run 01-Fit-meta-analysis.R to fit and store models of two-step meta analysis")
}

# Load fitted models
load("DATA_FITTED_MODELS/model-rem-diag-allS-3m.Rdata"); fitM3 <- random2Eight
load("DATA_FITTED_MODELS/model-rem-diag-allS-5m.Rdata"); fitM5 <- random2Eight
load("DATA_FITTED_MODELS/model-rem-diag-allS-7m.Rdata"); fitM7 <- random2Eight
load("DATA_FITTED_MODELS/model-rem-diag-allS-8m.Rdata"); fitM8 <- random2Eight

# Extract factor loadings from 8-task model
meta8Loadings <- coef(fitM8$mx.fit)
names(meta8Loadings) <- gsub("Alpha_", "", names(meta8Loadings) )

# Calculate CI score from scaled individual task scores
# average, weighted z-score: multiply * 15 + 100
m$T1 <- scale(m$BrainstormObject1) * meta8Loadings["BrainstormObject1"]
m$T2 <- scale(m$BrainstormWords1)  * meta8Loadings["BrainstormWords1"]
m$T3 <- scale(m$MatrixSolving1)    * meta8Loadings["MatrixSolving1"]
m$T4 <- scale(m$MemoryPicture1)    * meta8Loadings["MemoryPicture1"]
m$T5 <- scale(m$Sudoku1)           * meta8Loadings["Sudoku1"]
m$T6 <- scale(m$TypingNumbers1)    * meta8Loadings["TypingNumbers1"]
m$T7 <- scale(m$TypingText1)       * meta8Loadings["TypingText1"]
m$T8 <- scale(m$UnscrambleWords1)  * meta8Loadings["UnscrambleWords1"]
m$CI <- unlist( apply(m[ paste("T", 1:8, sep="")], 1, mean, na.rm=TRUE)) * 15 + 100

## Create subset with only complete cases for regression analysis and random forest
sub <- m[complete.cases( m[, c("CI", "GroupSize", "GroupSizeSq", "FemaleProportion", "RMEMean", "AgeDiversity", "TeamCongruence", "TeamCoverage", "TeamEffort", "SkillMax", "SkillAverage")] ), ]; dim(sub)
sub$StudyName <- as.factor(sub$StudyName)
dim(sub)


#####################################
## Overall Descriptives
#####################################
length( unique( m$StudyName ) )     # Number of studies       22
nrow( m )                           # Number of teams       1356
sum( m$GroupSize )                  # Number of individuals 5279


#####################################
## Table S1. Summary of samples and tasks in the data set.
#####################################
text <- xtable(desc, label="table_DescriptivesStudies", method="col.compact", caption="Table S1. Summary of samples and tasks in the data set.")
print.xtable(text, file="OUT/Table_S1.tex", size="scriptsize" )


#####################################
## Table S3. Pooled correlation table from first stage of meta-analysis.
#####################################
# random1Diag is first stage
# s object is summary() of the first stage
# Extract the coefficients and put them in a matrix. Then add "stars" to indicate significane level
tt <- vec2symMat( coef(random1Diag, select="fixed"), diag=FALSE )
pvals <- s$coefficients[ grepl("Intercept", rownames(s$coefficients)), "Pr(>|z|)"]
Signif <- getStars(pvals)
stars <- vec2symMat( paste("$^{", format(Signif), "}$", sep=""), diag=FALSE )
tt <- matrix( paste( format(tt, digits=1), stars, sep=""), nrow=8)

# Print table of first-stage pooled correlation table and use nicer labels
niceNames <- unname( taskLabels[allVars] )
niceNames <- paste(niceNames, " (", 1:length(niceNames), ")", sep="")
dimnames(tt) <- list(niceNames, paste("(", 1:length(niceNames), ")", sep=""))
tt[upper.tri(tt, TRUE)] <- NA
tt <- tt[, -(ncol(tt))]
text <- xtable(tt, caption="Table S3. Pooled correlation table from first stage of meta-analysis.", label="Stage1")
print.xtable(text, file="OUT/Table_S3.tex", size="scriptsize")


#####################################
## Table S4. Factor loadings of the second stage of the meta-analysis.
#####################################
screenreg( list(fitM3, fitM5, fitM7, fitM8), digits=2 )
texreg( list(fitM3, fitM5, fitM7, fitM8), "OUT/Table_S4.tex", digits=2, caption="Table S4. Factor loadings of the second stage of the meta-analysis." )


#####################################
## Table S5. Hierarchical linear regression predicting CI Score as a function of group size, demographic, process, and skill predictors. 
#####################################
tableS5 <- list(
  lm( CI ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion, sub),
  lm( CI ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean, sub),
  lm( CI ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity, sub),
  lm( CI ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort, sub),
  lm( CI ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, sub)
)
se   <- lapply(tableS5, function(x) { coeftest(x, df=Inf, vcov=vcovHC(x, type = "HC0"))[,2] })
pval <- lapply(tableS5, function(x) { coeftest(x, df=Inf, vcov=vcovHC(x, type = "HC0"))[,4] })
screenreg(tableS5, digits=2, include.rsq=FALSE, include.rmse=FALSE, omit.coef="StudyName", stars = c(0.01, 0.05, 0.1), override.se=se, override.pvalues=pval)
custom.coef.names <- c("(Intercept)", "Group Size", "Group Size$^2$", "Proportion Female", "Social Perceptiveness", "Age Diversity: \\textit{Medium}", "Age Diversity: \\textit{High}", "Congruence", "Coverage", "Effort", "Skill (Mean)")
texreg(tableS5, digits=2, omit.coef="StudyName", file="OUT/Table_S5.tex", dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE, include.rsq=FALSE, include.rmse=FALSE, 
       custom.coef.names= custom.coef.names, fontsize="scriptsize", stars = c(0.01, 0.05, 0.1), symbol="\\dagger", 
       override.se=se, override.pvalues=pval,
       caption="Table S5. Hierarchical linear regression predicting CI Score as a function of group size, demographic, process, and skill predictors. " )


#####################################
## Table S6. Hierarchical linear regression predicting CI Score and performance on each task separately
#####################################
models <- list(
  lm( 				CI ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, sub),
  lm(  BrainstormObject1 ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, sub),
  lm(   BrainstormWords1 ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, sub),
  lm(     MatrixSolving1 ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, sub),
  lm(     MemoryPicture1 ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, sub),
  lm(            Sudoku1 ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, sub),
  lm(     TypingNumbers1 ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, sub),
  lm(        TypingText1 ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, sub),
  lm(   UnscrambleWords1 ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, sub)
)
se   <- lapply(models, function(x) { coeftest(x, df=Inf, vcov=vcovHC(x, type = "HC0"))[,2] })
pval <- lapply(models, function(x) { coeftest(x, df=Inf, vcov=vcovHC(x, type = "HC0"))[,4] })
screenreg(models, digits=3, include.rsq=FALSE, include.rmse=FALSE, omit.coef="StudyName", stars = c(0.01, 0.05, 0.1), override.se=se, override.pvalues=pval)
custom.coef.names <- c("(Intercept)", "Group Size", "Group Size$^2$", "Proportion Female", "Social Perceptiveness", "Age Diversity: \\textit{Medium}", "Age Diversity: \\textit{High}", "Process: Congruence", "Process: Coverage", "Process: Effort", "Skill (Mean)")
texreg(models, digits=2, omit.coef="StudyName", file="OUT/Table_S6.tex", 
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE, include.rsq=TRUE, include.adjrs=FALSE, include.rmse=FALSE, 
       custom.coef.names= custom.coef.names, fontsize="scriptsize", stars = c(0.01, 0.05, 0.1), symbol="\\dagger", 
       override.se=se, override.pvalues=pval, 
       caption="Table S6. Hierarchical linear regression predicting CI Score and performance on each task separately." )


#####################################
## Table S7. Hierarchical linear regression sample split by collaboration modality (Models 1 & 2) and sample setting (Models 3 & 4).
#############################
fit <- 	lm( CI ~ StudyName + CollabStyle:GroupSize + CollabStyle:I(GroupSize^2) + CollabStyle:FemaleProportion + CollabStyle:RMEMean + CollabStyle:AgeDiversity + CollabStyle:TeamCongruence + CollabStyle:TeamCoverage + CollabStyle:TeamEffort + CollabStyle:SkillAverage, m )
linearHypothesis( fit, "CollabStyleColocated:RMEMean = CollabStyleRemote:RMEMean", white.adjust="hc0" )					      # 0.1242
linearHypothesis( fit, "CollabStyleColocated:TeamCongruence = CollabStyleRemote:TeamCongruence", white.adjust="hc0" )	# 0.4174
linearHypothesis( fit, "CollabStyleColocated:TeamCoverage = CollabStyleRemote:TeamCoverage", white.adjust="hc0" )		  # 0.1529
linearHypothesis( fit, "CollabStyleColocated:TeamEffort = CollabStyleRemote:TeamEffort", white.adjust="hc0" )			    # 0.165
linearHypothesis( fit, "CollabStyleColocated:SkillAverage = CollabStyleRemote:SkillAverage", white.adjust="hc0" )		  # 0.0001965


fit <- 	lm( CI ~ StudyName + Setting:GroupSize + Setting:I(GroupSize^2) + Setting:FemaleProportion + Setting:RMEMean + Setting:AgeDiversity + Setting:TeamCongruence + Setting:TeamCoverage + Setting:TeamEffort + Setting:SkillAverage, m )
linearHypothesis( fit, "Settingfield:RMEMean = Settinglab:RMEMean", white.adjust="hc0" )			                        # 0.8413
linearHypothesis( fit, "Settingfield:TeamCoverage = Settinglab:TeamCoverage", white.adjust="hc0" )	                  # 0.1652
linearHypothesis( fit, "Settingfield:TeamEffort = Settinglab:TeamEffort", white.adjust="hc0" )		                    # 0.07239
linearHypothesis( fit, "Settingfield:SkillAverage = Settinglab:SkillAverage", white.adjust="hc0" )	                  # 0.5456


models <- list(
  lm( CI ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, m[m$CollabStyle=="Colocated",] ),
  lm( CI ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, m[m$CollabStyle=="Remote",] ),
  lm( CI ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, m[m$Setting=="lab",] ),
  lm( CI ~ StudyName + GroupSize + I(GroupSize^2) + FemaleProportion + RMEMean + AgeDiversity + TeamCongruence + TeamCoverage + TeamEffort + SkillAverage, m[m$Setting=="field",] )
)
se   <- lapply(models, function(x) { coeftest(x, df=Inf, vcov=vcovHC(x, type = "HC0"))[,2] })
pval <- lapply(models, function(x) { coeftest(x, df=Inf, vcov=vcovHC(x, type = "HC0"))[,4] })
screenreg(models, digits=2, include.rsq=FALSE, include.rmse=FALSE, omit.coef="StudyName", stars = c(0.01, 0.05, 0.1), override.se=se, override.pvalues=pval)
custom.coef.names <- c("(Intercept)", "Group Size", "Group Size$^2$", "Proportion Female", "Social Perceptiveness", "Age Diversity: \\textit{Medium}", "Age Diversity: \\textit{High}", "Process: Congruence", "Process: Coverage", "Process: Effort", "Skill (Mean)")
texreg(models, digits=2, omit.coef="StudyName", file="OUT/Table_S7.tex", 
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE, include.rsq=FALSE, include.adjrs=TRUE, include.rmse=FALSE, 
       fontsize="scriptsize", stars = c(0.01, 0.05, 0.1), symbol="\\dagger", 
       override.se=se, override.pvalues=pval, custom.coef.names=custom.coef.names, 
       caption="Table S7. Hierarchical linear regression sample split by collaboration modality (Models 1 & 2) and sample setting (Models 3 & 4)." )


#####################################
## Table S8. Mediation analysis of social perceptiveness and proportion female
#############################
models <- list(
  lm(      CI ~ StudyName + GroupSize + I(GroupSize^2) + AgeDiversity + FemaleProportion, sub),
  lm( RMEMean ~ StudyName + GroupSize + I(GroupSize^2) + AgeDiversity + FemaleProportion, sub),
  lm(      CI ~ StudyName + GroupSize + I(GroupSize^2) + AgeDiversity + FemaleProportion + RMEMean, sub)
)
se   <- lapply(models, function(x) { coeftest(x, df=Inf, vcov=vcovHC(x, type = "HC0"))[,2] })
pval <- lapply(models, function(x) { coeftest(x, df=Inf, vcov=vcovHC(x, type = "HC0"))[,4] })
screenreg(models, digits=2, include.rsq=FALSE, include.rmse=FALSE, omit.coef="StudyName", stars = c(0.01, 0.05, 0.1), override.se=se, override.pvalues=pval)
custom.model.names <- c("Step 1: X on Y", "Step 2: X on M", "Step 3: X + M on Y")
custom.coef.names <- c("(Intercept)", "Group Size", "Group Size$^2$", "Age Diversity: \\textit{Medium}", "Age Diversity: \\textit{High}", "Proportion Female", "Social Perceptiveness")
texreg(models, digits=2, omit.coef="StudyName", file="OUT/Table_S8.tex", 
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE, include.rsq=FALSE, include.adjrs=TRUE, include.rmse=FALSE, 
       fontsize="scriptsize", stars = c(0.01, 0.05, 0.1), symbol="\\dagger", 
       override.se=se, override.pvalues=pval, 
       custom.coef.names=custom.coef.names, custom.model.names=custom.model.names, 
       caption="Table S8. Mediation analysis of social perceptiveness and proportion female" )


set.seed(1)
med <- mediation::mediate(models[[2]], models[[3]], treat='FemaleProportion', mediator='RMEMean', boot=TRUE, sims=1000)
summary(med)

## Run all the other mediation tests manually by changing the variables in the model and then the medeiate() test
## to generate Table S9. Summary of mediation analyses
## Female -> RME             -> CI ACME=0.35 [ 0.15 -  0.63]  <2e-16 ***
## Female -> TeamCongruence  -> CI ACME=0.03 [-0.08 -  0.18]   0.544  
## Female -> TeamCoverage    -> CI ACME=0.63 [ 0.23 -  1.07]  <2e-16 ***
## Female -> TeamEffort      -> CI ACME=0.27 [ 0.03 -  0.52]   0.022 *
## RME    -> TeamCongruence  -> CI ACME=0.00 [-0.01 -  0.02]    0.89
## RME    -> TeamCoverage    -> CI ACME=0.06 [ 0.02 -  0.12]   0.014 *  
## RME    -> TeamEffort      -> CI ACME=0.03 [ 0.01 -  0.07]   0.022 *  


#########################################
## Figure 2A -- Pooled Correlations
#########################################
melt <-  m[,c("CI", "BrainstormObject1", "BrainstormWords1", "MatrixSolving1", "MemoryPicture1", "Sudoku1", "TypingNumbers1", "TypingText1", "UnscrambleWords1")]
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
M <- cor(melt, use="pairwise.complete.obs")
row.names(M) <- colnames(M) <- c("CI", "Brainstorm Object", "Brainstorm Words", "Matrix Reasoning", "Memory Pictures", "Sudoku", "Typing Numbers", "Typing Text", "Unscrable Words")
p.mat <- cor.mtest(melt)	# matrix of the p-value of the correlation
max(p.mat$p)     # "all correlations are significant with at least p < X"
corrplot(M, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=45, 
         tl.cex=1.1,
         cl.lim=c(-.2, 1),
         cl.length=5,
         cl.cex=1.2, 
         # Combine with significance
         p.mat = p.mat$p, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
dev.copy(cairo_pdf, file="FIGURES/Figure_2A.pdf", width=8, height=8); dev.off()

## Quantities of interest mentioned in the paper: Average correlation and range
M[upper.tri(M, diag=TRUE)] <- NA
M[, "CI"] <- NA
mean(M, na.rm=TRUE)
range(M, na.rm=TRUE)


############################################
## Figure 2B: Factor Loading Radar Bar Plot
############################################
melt <- data.frame( Task=c("Brainstorm Object", "Brainstorm Words", "Matrix Reasoning", "Memory Pircutre", "Sudoku", "Typing Numbers", "Typing Text", "Unscramble Words"),
                    Loading=coef(random2Eight), 
                    stringsAsFactors=FALSE)
ggplot(melt, aes( Task, Loading, fill=Task)) +
  geom_bar(stat="identity", width = .6) + 
  coord_polar() + 
  labs(x = "", y = "") + 
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 0.5, by=0.1)) +
  theme(legend.position = "none", axis.ticks=element_blank())
ggsave("FIGURES/Figure_2B.pdf", width=7, height=7, device=cairo_pdf)


###############################
## Figure 2C -- Leave one out correlations
###############################
# Load all the leave-one out fitted models
out <- lapply( tasks, function(task) {
  file <- paste0("DATA_FITTED_MODELS/model-rem-diag-allS-8m_new_Excl", task, ".Rdata")
  load( file )
  meta8ExclOne <- coef(random2Eight$mx.fit)
  m$CIExclOne <- as.numeric( scale( m[, gsub("Alpha_", "", names(meta8ExclOne) )]) %*% meta8ExclOne  * 15 + 100 )
  m[, paste0("CIExcl", task)] <<- m$CIExclOne
  cc <- cor.test(m$CIExclOne, m[, task], conf.level = 0.95)
  data.frame( Exclude=task, Estimate=cc$estimate, Lower=cc$conf.int[1], Upper=cc$conf.int[2], stringsAsFactors=FALSE )	
})
out <- rbindlist(out)
out$Label <- tasksPretty
out <- out[order(-out$Estimate),]
out$Label <- factor( out$Label, levels=out$Label)

# Average correlation
mean(out$Estimate)    # 0.3945912
range(out$Estimate)

ggplot(out, aes(x=Label, y=Estimate)) +
  geom_linerange(aes(ymin=Lower, ymax=Upper)) +
  geom_point(shape=18, size=4, color="#1b9e77") +
  theme_classic() +
  ylim( c(0, .63)) + 
  theme(  axis.text.x = element_text(angle = 45, hjust = 1, size=15), 
          axis.title.y = element_text(size=16), 
          axis.text.y = element_text(size=15), 
          panel.grid.major.y = element_line(color = "grey80")) +
  labs(x=NULL, y="Correlation of leave-one-out CI\nwith criterion task")
ggsave("FIGURES/Figure_2C.pdf", width=5.0, height=5.0)


############################################
## Figure 2D: Coefficient Plot
############################################
p <- plot_summs(tableS5[[1]], tableS5[[2]], tableS5[[3]], tableS5[[5]], robust="HC0", scale = TRUE, legend.title="", model.names=c("Model 1", "Model 2", "Model 3", "Model 5"),
           coefs = c( #"Group Size"            = "GroupSize",
             "Proportion Female"     = "FemaleProportion", 
             "Social Perceptiveness" = "RMEMean", 
             "Age Diversity: Medium" = "AgeDiversityMedium", 
             "Age Diversity: High"   = "AgeDiversityHigh", 
             "Process: Skill Congruence"   = "TeamCongruence", 
             "Process: Strategy"     = "TeamCoverage", 
             "Process: Effort"       = "TeamEffort", 
             "Skill (Mean)"           = "SkillAverage")) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.text=element_text(size=12, colour="black"), 
        legend.title=element_text(colour="black"),
        axis.title = element_text(size=13, colour="black"), 
        axis.text=element_text(size=12, colour="black"), 
        axis.text.x=element_text(size=12, colour="black"), 
        axis.text.y=element_text(size=12, colour="black", hjust=0), 
        axis.line=element_line(),
        legend.position="bottom")
print(p)
dev.copy(cairo_pdf, file="FIGURES/Figure_2D.pdf", width=5.0, height=5.0); dev.off()


############################################
## Figure 3: Variable Importance 
##    ==> see 03-Variable-Importance.R
############################################


############################################
## Figure S1: Estimated criterion task performance for teams with one standard deviation increase in the restricted CI-ct store
############################################
set.seed(1)
out <- lapply( tasks, function(task) {
  restrictedCI <- paste0("CIExcl", task)
  m$Y  <- m[, task]
  m$xZ <- as.numeric( scale(m[, restrictedCI]) )
  
  # Plain model, no study FE
  fit <- lm( Y ~ xZ, m)
  newData <- data.frame( '(Intercept)'=1, xZ=1)
  
  getSimData(newData, fit, label=as.character(tasksPretty[task]) )
})
melt <- rbindlist(out)
agg <- aggregate( Improvement ~ Label, melt, mean); names(agg) <- c("Label", "groupMean")
melt <- merge(melt, agg)

## Quantity of interest for text
mean(melt$Improvement) 			  # improvment of x %
IQR(melt$Improvement) * 1.5		# plus minus 

# Sort factor by improvement
melt$Label <- factor(melt$Label, levels=agg$Label[order(agg$groupMean)] )

## PLOT
ggplot(melt, aes(x=Label, y=Improvement)) +
  stat_boxplot(geom = "errorbar", width=0.3, size=.1) +
  geom_boxplot(aes(fill=groupMean), color="#999999", size=.2, width=.5) +
  scale_fill_gradient2(low="#d9dce8", mid="#a18ab2", high="#7c4062", guide=FALSE) +
  theme_classic() +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1L)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme(  axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin =  ggplot2::margin(.5, 0, 0, 0, "cm") ) +
  labs(x="Criterion Task", y=expression( paste( atop("% improvement in performance", "resulting from one SD increase in CI"[-ct]) ) ) )
ggsave("FIGURES/Figure_S1.pdf", width=4.0, height=3.5)


############################################
## Figure S2: Variable Importance by task 
##     ==> see 03-Variable-Importance.R
############################################

