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

	headerFields = c(Accept = "text/xml", Accept = "multipart/*", "Content-Type" = "text/xml; charset=utf-8", SOAPAction = paste("wssql/", action, "?ver_=3.05", sep=""))

	url <- ""
	url <- paste("http://", userId , ":", password , "@", host, ":8510/", sep="")

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

r2hpcc.HTTPRequest2 <- function(host, port, urn, params, headers = NULL, body = NULL)
{
	args <- ""
	if (!is.null(params) & length(params) > 0)
	{
		for (param in names(params))
		{
			if (nchar(args) == 0)
			{
				args <- paste(args, "?", sep = "")
			}
			else
			{
				args <- paste(args, "&", sep = "")
			}

			args <- paste(args, param, "=", params[[param]], sep = "")
		}
	}

	url <- ""
	url <- paste("http://", host, ":", port, "/", urn, args, sep = "")
	print(url)

	reader = basicTextGatherer()

	handle = getCurlHandle()

	headerFields <- list()
	headerFields[["Accept"]] <- "application/json"
	headerFields[["Content-Type"]] <- "application/x-www-form-urlencoded"
	headerFields[["Referer"]] <- paste("http://", host, ":", port, "/", sep = "")
	headerFields[["Accept-Encoding"]] <- "gzip, deflate"

	if (!is.null(headers) & length(headers) > 0)
	{
		for (header in names(headers))
		{
			headerFields[[header]] <- headers[[header]]
		}
	}
	print(headerFields)


	if(is.null(body))
	{

		curlPerform(url = url,
				httpheader = headerFields,
				writefunction = reader$update,
				curl = handle)
	}
	else
	{
		curlPerform(url = url,
				httpheader = headerFields,
				postfields = body,
				writefunction = reader$update,
				curl = handle)
	}

	status = getCurlInfo(handle)$response.code
	varWu1 <- reader$value()
	txt <- gsub("&lt;", "<", varWu1)
	txt <- gsub("&gt;", ">", txt)
	txt <- gsub("&apos;", "'", txt)
	txt <- gsub("&quot;", "\"", txt)
	txt
}