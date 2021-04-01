##############################################
## Figure 3: Variable importance in predicting collective intelligence.
##
## Creat a data-driven variable imporatance plot based on random forrests. 
## Following approach outlined on page 319 in LISR
##############################################

# Pick only groups for which data is complete
sub <- m[complete.cases( m[, c("CI", "GroupSize", "GroupSizeSq", "FemaleProportion", "RMEMean", "AgeDiversity", "TeamCongruence", "TeamCoverage", "TeamEffort", "SkillMax", "SkillAverage")] ), ]; dim(sub)

set.seed(1)
rf <- randomForest(CI ~ GroupSize + GroupSizeSq + FemaleProportion + AgeDiversity + SkillAverage + SkillMax + RMEMean + TeamCongruence + TeamCoverage + TeamEffort, data=sub, importance=TRUE, mtry=3, ntree=500)
rf

melt <- as.data.frame( rf$importance ); melt[,1] <- NULL; names(melt) <- c("VarImp")	# Use node impority as measure, as does LISR
mapping <- c(GroupSize="Group Size", GroupSizeSq="Group Size", FemaleProportion="Composition", AgeDiversity="Composition", SkillAverage="Skill", SkillMax="Skill", RMEMean="Social Perceptiveness", 
			TeamCongruence="Process", TeamCoverage="Process", TeamEffort="Process")
melt$Variable <- mapping[ row.names(melt) ]
melt <- aggregate( VarImp ~ Variable, melt, sum)
melt$Perc <- (melt$VarImp / sum( melt$VarImp )) * 100			  # Percentage for stacked graph
melt$RelToMax <- (melt$VarImp / max( melt$VarImp )) * 100		# Following ISLR, p319 "Variable importance is computed using the mean decrease in Gini index, 
																                            # 		and expressed relative to the maximum."
melt <- melt[order(melt$RelToMax),]
melt$Variable <- factor(melt$Variable, levels=melt$Variable)
ggplot(melt, aes(x=Variable, y=RelToMax)) +
	geom_bar(stat="identity", fill="#7fc97f") +
	coord_flip() +
	theme_classic() +
	theme(panel.grid.major.x = element_line(color = "grey80")) +
	labs(x=NULL, y="Variable Importance")
scale <- .9
ggsave("FIGURES/Figure_3.pdf", width=4.5*scale, height=2.0*scale)


##############################################
## Fig. S2. Variable importance by task.
##############################################

## BY TASK -- CI
sub <- m[complete.cases( m[, c("CI", "GroupSize", "GroupSizeSq", "FemaleProportion", "RMEMean", "AgeDiversity", "TeamCongruence", "TeamCoverage", "TeamEffort", "SkillMax", "SkillAverage")] ), ]; dim(sub)
names(sub)[names(sub)=="SkillAverage"] <- "SkillMean"
melt0 <- getVarImpMelt(sub, "CI")

## BY TASK -- Matrix Reasoning
pt <- read.csv("DATA/byTask/taskMatrix.csv", stringsAsFactors=FALSE)
sub <- merge (m[, c("TeamID", "GroupSize", "GroupSizeSq", "FemaleProportion", "AgeDiversity", "RMEMean")], pt)
sub <- sub[!is.na(sub$RMEMean),]
melt1 <- getVarImpMelt(sub, "Matrix Reasoning")

## BY TASK -- Sudoku
pt <- read.csv("DATA/byTask/taskSudoku.csv", stringsAsFactors=FALSE)
sub <- merge (m[, c("TeamID", "GroupSize", "GroupSizeSq", "FemaleProportion", "AgeDiversity", "RMEMean")], pt)
sub <- sub[!is.na(sub$RMEMean),]
melt2 <- getVarImpMelt(sub, "Sudoku")

## BY TASK -- Typing Text
pt <- read.csv("DATA/byTask/taskText.csv", stringsAsFactors=FALSE)
pt$congruence <- 1	# should all be 1 but one value is 0.9999
sub <- merge (m[, c("TeamID", "GroupSize", "GroupSizeSq", "FemaleProportion", "AgeDiversity", "RMEMean")], pt)
sub <- sub[!is.na(sub$RMEMean),]
melt3 <- getVarImpMelt(sub, "Typing Text")

## BY TASK -- Unscramble Words
pt <- read.csv("DATA/byTask/taskUnscramble.csv", stringsAsFactors=FALSE)
sub <- merge (m[, c("TeamID", "GroupSize", "GroupSizeSq", "FemaleProportion", "AgeDiversity", "RMEMean")], pt)
sub <- sub[!is.na(sub$RMEMean),]
melt4 <- getVarImpMelt(sub, "Unscramble Words")

## BY TASK -- Brainstorm Object
pt <- read.csv("DATA/byTask/taskBrick.csv", stringsAsFactors=FALSE)
pt$congruence <- 1	# should all be 1 but one value is 0.9999
sub <- merge (m[, c("TeamID", "GroupSize", "GroupSizeSq", "FemaleProportion", "AgeDiversity", "RMEMean")], pt)
sub <- sub[!is.na(sub$RMEMean),]
melt5 <- getVarImpMelt(sub, "Brainstorm Object")

## Put everything together in a single figure
melt <- rbind(melt0, melt1, melt2, melt3, melt4, melt5)
melt$Variable <- factor(melt$Variable, levels=c("Composition", "Social Perceptiveness", "Group Size", "Skill", "Process") )
melt$What <- factor( melt$What, 
					 levels=c("CI", "Brainstorm Object", "Matrix Reasoning", "Sudoku", "Typing Text", "Unscramble Words" ) )

ggplot(melt, aes(x=What, y=Perc, fill=Variable)) +
	geom_bar(stat="identity", width=.6) +
	theme_classic() +
	theme(	panel.grid.major.y = element_line(color = "grey80"),
			axis.text.x = element_text(angle = 45, hjust = 1),
			legend.position="right") +
	labs(x=NULL, y="Variable Importance") + 
	scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
	scale_fill_brewer(name=NULL, palette="Pastel1")		# PuBuGn
ggsave("FIGURES/Figure_S2.pdf", width=6.5, height=3.0)
