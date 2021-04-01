#################################
## Helper function: Fit random forest and extract variable importance
#################################
getVarImpMelt <- function(dat, label) {
  set.seed(1)
  rf <- randomForest(CI ~ GroupSize + GroupSizeSq + FemaleProportion + AgeDiversity + SkillMean + SkillMax + RMEMean + TeamCongruence + TeamCoverage + TeamEffort, 
                     data=dat, importance=TRUE, mtry=3, ntree=500)
  melt <- as.data.frame( rf$importance ); melt[,1] <- NULL; names(melt) <- c("VarImp")	
  mapping <- c(GroupSize="Group Size", GroupSizeSq="Group Size", FemaleProportion="Composition", 
               AgeDiversity="Composition", SkillMean="Skill", SkillAverage="Skill", SkillMax="Skill", RMEMean="Social Perceptiveness", 
               TeamCongruence="Process", TeamCoverage="Process", TeamEffort="Process")
  melt$Variable <- mapping[ row.names(melt) ]
  melt <- aggregate( VarImp ~ Variable, melt, sum)
  melt$Perc <- (melt$VarImp / sum( melt$VarImp ))			# Percentage for stacked graph
  melt$What <- label
  melt
}

