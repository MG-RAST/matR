
#######################################################
### test matrix retrieval across all parameters
### 
### 
#######################################################

MIDs <- mMetagenomes ()
small <- MIDs [sample (length (MIDs), 2)]

orgMatrix (small)
orgMatrixEvalue (small)
orgMatrixLength (small)
orgMatrixPercentID (small)

funcMatrix (small)
funcMatrixEvalue (small)
funcMatrixLength (small)
funcMatrixPercentID (small)

