library(matR)

#-----------------------------------------------------------------------------------------
#  parMapper() is the workhorse enabling user calls such as:
#
#  princomp(xx,
#    map = c (
#      col="host_common_name",
#      pch="samp_store_temp"),
#    col = c (
#      Mouse="blue",
#      cow="red",
#      "striped bass"="brown"),
#    pch = c (
#      "-80"="+",
#      "NA"="x"))
#-----------------------------------------------------------------------------------------
parMapper (xx1, 
		name.map = c (
			col="host_common_name",
			pch="samp_store_temp"),
		value.map = list (
			pch = c (
				"-80"="+",
				"NA"="x"),
			extraneous = 5000))
parMapper (xx1, 
		name.map = c (
			col="host_common_name",
			pch="samp_store_temp"),
		value.map = list (
			col = c (Mouse="blue"),
			pch = c (
				"-80"="+",
				"NA"="x"),
			extraneous = 5000))
parMapper (xx1, 
		name.map = c (
			col="host_common_name",
			pch="samp_store_temp"),
		value.map = list (
			col = c (
				Mouse="blue",
				cow="red"),
			pch = c (
				"-80"="+",
				"NA"="x"),
			extraneous = 5000))
parMapper (xx1, 
		name.map = c (
			col="host_common_name",
			pch="samp_store_temp"),
		value.map = list (
			col = c (
				Mouse="blue",
				"NA"="brown"),
			pch = c (
				"-80"="+",
				"NA"="x"),
			extraneous = 5000))
