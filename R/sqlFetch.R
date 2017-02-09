#' Calls the executesql function of ws_sql
#'
#' Takes in two parameters HPCC filename and HPCC connection object
#' Returns a data frame.
#' If the function eencountered any error while executing the query, 2nd element will have a value of -1 and 1st element, the error message.
#'
#' @param con - hpcc connection information
#' @param filename - filename to fetch from HPCC
#' @param limit - limit number of rows to retrieve (defaults to connection level if not provided)
#' @export 
r2hpcc.sqlFetch <- function (Con, filename,limit) {
  
  host <- Con[1]
  targetCluster <- Con[2]
  userId <- Con[3]
  password <- Con[4]
  resultLimit <- if (missing(limit)) {Con[5]} else {limit}
  
  query <- paste('SELECT * FROM ',filename,sep='')

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
  newlst <- xmlParse(txt)
  layout <- getNodeSet(newlst, "//*[local-name()='Result']",
                       namespaces = xmlNamespaceDefinitions(newlst, simplify = TRUE))

  # Query Proccessed successfully
  if (length(layout) > 0) {
    colLayout <<- layout[[1]]
    l1 <<- xmlToList(colLayout)

    #get the row count
    rCount <- l1[2]$Dataset$Row$WSSQLSelectQueryResultCount

    # Remove the attrib element from the list
    l1 <- l1[1]$Dataset[-length(l1[1]$Dataset)]

    df<-do.call(rbind.data.frame, l1)
    row.names(df) <- NULL
    df
    } else { # "error"
    layout <- getNodeSet(newlst, "//*[local-name()='Exception']",
                         namespaces = xmlNamespaceDefinitions(newlst, simplify = TRUE))
    colLayout <<- layout[[1]]
    l1 <<- xmlToList(colLayout)
    l2 <- data.frame(Code = l1$Code,ErrorMessage = substr(l1$Message,1,70))
    l2
  }
}
