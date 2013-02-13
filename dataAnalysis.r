#
# The purpose of your analysis is to identify and quantify associations between the interest rate of 
# the loan and the other variables in the data set. In particular, you should consider whether any of 
# these variables have an important association with interest rate after taking into account the 
# applicant's FICO score. For example, if two people have the same FICO score, can the other variables 
# explain a difference in interest rate between them?
#

getDataFrame <- function( workingDirectory, dataFile )
{
  pDataFile <- paste0( workingDirectory, dataFile )
  return( read.table( pDataFile, sep=",", header=TRUE ) )
}

loanData <- getDataFrame( getwd(), "/loansData.csv" )

library( stringr )
ficoScore <- unique( str_split_fixed( loanData$FICO.Range, "-", 2 ) )

#
# This section is just to get a table of FICO scores, listing original score name,
# example: 640-644. It also breaks that value into two columns, example: 640  644.
# It then finds the number of FICO scores in data that match that range
#

# Split out FICO scores into separate columns
tmpFicoScores <- matrix( unlist( strsplit( as.character( loanData$FICO.Range ), '-' ) ), ncol=2, byrow=TRUE)
# Store FICO scores, original value plus separated columns into dataframe
ficoScoreFrame <- cbind( loanData$FICO.Range, as.data.frame( tmpFicoScores ) )
# Add in a new top row for column names
names(ficoScoreFrame) <- c( "original", "lower_bound_score", "upper_bound_score")
# Order the FICO scores from lower to upper bound
ficoScoreFrame <- ficoScoreFrame[order(ficoScoreFrame$lower_bound_score),]
# Count the number of unique FICO scores that match the lower to upper bound ranges in the data
ficoUniqueCounts <- tapply( ficoScoreFrame$original, list( ficoScoreFrame$lower_bound_score ), length )
# Save off only the unique FICO scores
ficoScoreFrame <- unique( ficoScoreFrame )
# Dynamically add in the column (called "count") of unique FICO scores into main data frame
ficoScoreFrame$count <- ficoUniqueCounts
# Converting lower and upper bound scores into class "numeric" 
ficoScoreFrame$lower_bound_score <- as.numeric( as.character( ficoScoreFrame$lower_bound_score ) )
ficoScoreFrame$upper_bound_score <- as.numeric( as.character( ficoScoreFrame$upper_bound_score ) )
# Simple plot
plot( ficoScoreFrame$lower_bound_score, ficoScoreFrame$count, pch=19, col="blue", xlab="FICO Score", 
      ylab="Sum of FICO Scores In Range", lwd=5, main = "FICO Scores", 
      xlim = range( ficoScoreFrame$lower_bound_score,  ficoScoreFrame$lower_bound_score[nrow(ficoScoreFrame)] ) )

#Test
#Interest.rate