# communications parameters
APIAuth <- ""
APIServers <- list (
	prod = "http://api.metagenomics.anl.gov/",
	prod2 = "http://metagenomics.anl.gov/api.cgi/",
	dev = "http://dev.metagenomics.anl.gov/api.cgi/",
	dev2 = "http://dunkirk.mcs.anl.gov/~paczian/mgrastv3/api.cgi/",
	dev3 = "http://dev.metagenomics.anl.gov/api_new.cgi/",
	shock = "http://shock.mcs.anl.gov")
APIServer <- APIServers$prod

# MG-RAST info
# ... this is close, but not exactly right ...
.mNamespaces <- list (M5NR = "M5NR", SwissProt = "SwissProt", GenBank = "GenBank", IMG = "IMG", SEED = "SEED",
	TrEMBL = "TrEMBL", RefSeq = "RefSeq", PATRIC = "PATRIC", eggNOG = "eggNOG", KEGG = "KEGG", RDP = "RDP",
	Greengenes = "Greengenes", LSU = "LSU", SSU = "SSU")