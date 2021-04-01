###############################################
## extract.wls
##
## Custom extraction function for {texreg} and tssem2() models from {metaSEM}
##
## by Christoph Riedl <c.riedl@neu.edu>
###############################################

extract.wls <- function(model) {	s <- summary(model)	names <- rownames(s$coef)	co <- s$coef[, 1]	se <- s$coef[, 2]	pval <- s$coef[, 6]

	chi      <- s$stat["Chi-square of independence model",1]
	df       <- s$stat["DF of independence model",1]
	#chi.pval <- s$stat["p value of target model",1]#; if(pval < .0001) pval <- "< .0001"
	rmsea    <- s$stat["RMSEA",1]
	rmseall  <- s$stat["RMSEA lower 95% CI",1]
	rmseaul  <- s$stat["RMSEA upper 95% CI",1]
	cfi      <- s$stat["CFI",1]
		n <- s$stat["Sample size",1]
	
	# Compute average variance extracted
	# Based on: http://openmx.psyc.virginia.edu/thread/3988
	# Could also check description of reliability() from {semTools}
	mat <- model$mx.fit$impliedS1$result
	ave <- mean( mat[nrow(mat), -ncol(mat)] )
	
	gof <- c(chi, df, rmsea, rmseall, rmseaul, cfi, ave, n)	gof.names <- c("Chi-square of independence model", "DF of independence model", "RMSEA", "RMSEA lower 95% CI", "RMSEA upper 95% CI", "CFI", "Average variance extracted", "Num.\\ obs.")
    gof.decimal <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

	tr <- createTexreg(		coef.names = names,		coef = co,		se=se,		pvalues = pval,		gof.names = gof.names,		gof = gof,
		gof.decimal = gof.decimal
	)	return(tr)
}
setMethod("extract", signature = className("wls", "metaSEM"), definition = extract.wls)
