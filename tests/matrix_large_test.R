
dontrun <- function () {
#######################################################
### test matrix retrieval across all parameters
### 
### 
#######################################################

# MIDs <- mMetagenomes ()

small <- sample (length (MIDs), 2)
medium <- sample (length (MIDs), 20)
large <- sample (length (MIDs), 200)
xlarge <- sample (length (MIDs), 2000)


orgMatrix ()
orgMatrixEvalue ()
orgMatrixLength ()
orgMatrixPercentID ()

funcMatrix ()
funcMatrixEvalue ()
funcMatrixLength ()
funcMatrixPercentID ()
}
