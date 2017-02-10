#' Calls the executesql function of ws_sql
#'
#' Takes in two parameters HPCC SQL statement and HPCC connection object
#' Returns a data frame.
#' If the function eencountered any error while executing the query, 2nd element will have a value of -1 and 1st element, the error message.
#'
#' @param conn - hpcc connection information
#' @param query - SQL statement to execute
#' @param limit - limit number of rows to retrieve (defaults to connection level if not provided)
#' @export 
r2hpcc.ExecuteSQL <- function(conn, query, limit)
{
  host <- conn[1]
  targetCluster <- conn[2]
  userId <- conn[3]
  password <- conn[4]
  resultLimit <- if (missing(limit)) {conn[5]} else {limit}
  debugMode <- conn[6]

  body <- ""
  body <- paste('<?xml version="1.0" encoding=""?>
                 <soap:Envelope xmlns="urn:hpccsystems:ws:wssql" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
                 <soap:Body>
                 <ExecuteSQLRequest>
                 <SqlText>', query  , '</SqlText>
                 <UserName>', userId , '</UserName>
                 <TargetCluster>', targetCluster ,'</TargetCluster>
                 <TargetQuerySet/>
                 <SuppressResults>0</SuppressResults>
                 <SuppressXmlSchema>1</SuppressXmlSchema>
                 <IgnoreCache>1</IgnoreCache>
                 <Wait>-1</Wait>
                 <resultLimit>', resultLimit , '</resultLimit>
                 <ResultWindowStart>0</ResultWindowStart>
                 <ResultWindowCount>', resultLimit , '</ResultWindowCount>
                 </ExecuteSQLRequest>
                 </soap:Body>
                 </soap:Envelope>', sep="")

  reader = basicTextGatherer()

  handle = getCurlHandle()

  headerFields = c(Accept = "text/xml", Accept = "multipart/*",
                   `Content-Type` = "text/xml; charset=utf-8", SOAPAction = "urn:hpccsystems:ws:WsSQL")

  url <- ""
  url <- paste('http://', userId , ':', password , '@', host, ':8510/',  sep="")

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
  
  if (debugMode == TRUE)
  {
    print("DEBUG Message <SOAP Response>:")
    print(txt)
  }
  
  # Check for exception
  resp <- r2hpcc.Exception(conn, txt)
  
  # Query Proccessed successfully
  if (nchar(resp) == 0)
  {
    newlst <- xmlParse(txt)
    layout <- getNodeSet(newlst, "//*[local-name()='Result']",
                         namespaces = xmlNamespaceDefinitions(newlst, simplify = TRUE))
    
    if (debugMode == TRUE)
    {
      print("DEBUG Message <Result node>:")
      print(layout)
    }
    
    colLayout <<- layout[[1]]
    l1 <<- xmlToList(colLayout)

    if (debugMode == TRUE)
    {
      print("DEBUG Message <Result node converted to list>:")
      print(l1)
    }
    
    #get the row count
    rCount <- l1[2]$Dataset$Row$WSSQLSelectQueryResultCount

    # Remove the attrib element from the list (drop element from index marked with -) 
    l1 <- l1[1]$Dataset[-length(l1[1]$Dataset)]
    
    if (debugMode == TRUE)
    {
      print("l1:")
      print(l1)
    }
    
    df<-do.call(rbind.data.frame, l1)
    row.names(df) <- NULL
    df
  }
}
