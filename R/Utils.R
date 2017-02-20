#
r2hpcc.NVL <- function(arg1, arg2 = "")
{
	if (is.null(arg1))
	{
		resp <- arg2
	}
	else
	{
		resp <-arg1
	}
	resp
}


#
r2hpcc.HTTPRequest <- function(host, userId, password, action, body)
{
	reader = basicTextGatherer()
	
	handle = getCurlHandle()

	headerFields = c(Accept = "text/xml", Accept = "multipart/*", 'Content-Type' = "text/xml; charset=utf-8", SOAPAction = paste("wssql/", action, "?ver_=3.05", sep=""))

	url <- ""
	url <- paste('http://', userId , ':', password , '@', host, ':8510/', sep="")

	curlPerform(url = url,
				httpheader = headerFields,
				postfields = body,
				writefunction = reader$update,
				curl = handle)

	status = getCurlInfo(handle)$response.code
	varWu1 <- reader$value()
	txt <- gsub("&lt;", "<", varWu1)
	txt <- gsub("&gt;", ">", txt)
	txt <- gsub("&apos;", "'", txt)
	txt <- gsub("&quot;", "\"", txt)
	txt
}