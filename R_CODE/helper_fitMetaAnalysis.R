############################################################
## Helper function to fit 2-stage meta analysis based on configuration file
############################################################

# OpenMX Configuration
mxOption(NULL, "Number of Threads", 8)

m <- read.csv( mainDataFile, stringsAsFactors=FALSE); dim(m)

# Show what studies and observations are included
tt <- sort( table(m$StudyName), T)
tt
sum(tt)

# Get nicer labels for tasks
tasks <- read.csv("CONFIGURATION/tasks.csv", stringsAsFactors=FALSE)
taskLabels <- tasks$TaskLabel
names(taskLabels) <- tasks$Task

studies <- read.csv("CONFIGURATION/studies.csv", stringsAsFactors=FALSE)
studyLabels <- studies$StudyLabel
names(studyLabels) <- studies$StudyName

# Load the configuration file
source(inFile)

# Compute nObs
nObs <- unlist( lapply( names(variables), function(x) {nrow(m[m$StudyName==x,])}) ); names(nObs) <- names(variables)
allVars <- sort( unique(unlist(variables)) )

# How many observations in total?
sum(nObs)

# Compute data and descriptives table
dat <- list()
desc <- matrix(data="", nrow=length(allVars), ncol=length(names(variables)), dimnames=list(allVars, names(variables) ) )
type <- "corr"
for(s in names(variables)) {
	vars <- variables[[s]]
	mat <- matrix(data=NA, nrow=length(allVars), ncol=length(allVars), dimnames=list(allVars, allVars))
	for(v1 in vars) {
		for(v2 in vars) {
			x <- m[m$StudyName==s, v1]
			y <- m[m$StudyName==s, v2]
			if( length( na.omit(x))==0 | length(na.omit(y))==0) {
				stop( paste("Something is wrong", s, v1, v2))
			}
			if(type=="varcovar") {
				if(v1==v2) {
					r <- var(x, y)
				} else {
					r <- cov(x, y)
				}
			} else if(type=="corr") {
				# r <- cor(x, y)
				r <- cor.test( x, y )$estimate		# Variables don't need to be scaled
			} else {
				dat <- NULL
				stop("Invalid type")
			}
			mat[v1, v2] <- r
		}
		desc[v1, s] <- "X"
	}
	dat[[s]] <- mat
}
rownames(desc) <- unname( taskLabels[rownames(desc)] )
colnames(desc) <- unname( studyLabels[colnames(desc)] )

desc <- rbind(desc, 'Tasks in Study'=unname(apply(desc, 2, function(x) sum(x=="X"))), N=nObs, Total=sum(nObs) )
desc


########################
## STAGE 1
########################
## Fit initial random-effects model
random1Diag <- tssem1(Cov=dat, n=nObs, method="REM", RE.type="Diag")
# re-run model fit to get convergence
if(runRerun) {
  random1Diag <- rerun(random1Diag, autofixtau2 = TRUE)
}

(s <- summary(random1Diag))   # Despite message about conversion error, OpenMx status1: 0 ("0" or "1": The optimization is considered fine).

(tt <- vec2symMat( coef(random1Diag, select="fixed"), diag=FALSE ) )


########################
## STAGE 2 - Prepare Matrices
########################
## Factor covariance among latent factors
Phi <- matrix(c(1), ncol=1, nrow=1)

## Error covariance matrix
Psi <- Diag( paste("0.4*e", 1:length(allVars), sep="") )

## S matrix
S1 <- bdiagMat(list(Psi, Phi))

## This step is not necessary but it is useful for inspecting the model.
dimnames(S1)[[1]] <- dimnames(S1)[[2]] <- c(allVars, "Alpha") 

## A matrix
Lambda <- matrix(c(paste(".3*Alpha_", allVars, sep="")), ncol=1, nrow=length(allVars))
A1 <- rbind( cbind(matrix(0, ncol=length(allVars), nrow=length(allVars)), Lambda),
             matrix(0, ncol=length(allVars)+1, nrow=1) )

## This step is not necessary but it is useful for inspecting the model.
dimnames(A1)[[1]] <- dimnames(A1)[[2]] <- c(allVars, "Alpha") 

## F matrix to select the observed variables
F1 <- create.Fmatrix(c(rep(1, length(allVars)),0), as.mxMatrix=FALSE)



########################
## STAGE 2 - RUN
########################
random2Eight <- tssem2(random1Diag, Amatrix=A1, Smatrix=S1, Fmatrix=F1, diag.constraints=FALSE, intervals="z")
summary(random2Eight)
screenreg(random2Eight)

save(random2Eight, file=outFile)
