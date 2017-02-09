#' Calls the executesql function of ws_sql
#'
#' Takes in two parameters HPCC SQL statement and HPCC connection object
#' Returns a data frame.
#' If the function eencountered any error while executing the query, 2nd element will have a value of -1 and 1st element, the error message.
#'
#' @param con - hpcc connection information
#' @param query - SQL statement to execute
#' @param limit - limit number of rows to retrieve (defaults to connection level if not provided)
#' @export 
r2hpcc.Exception <- function(conn, SOAPResponse)
{
  debugMode <- conn[6]

  newlst <- xmlParse(SOAPResponse)
  layout <- getNodeSet(newlst, "//*[local-name()='Exception']",
                       namespaces = xmlNamespaceDefinitions(newlst, simplify = TRUE))

  # Query Proccessed successfully
  if (length(layout) > 0)
  {
    if(debugMode == TRUE)
    {
      print("DEBUG Message <Exception node>:")
      print(layout)
    }

    colLayout <<- layout[[1]]
    l1 <<- xmlToList(colLayout)

    if(debugMode == TRUE)
    {
      print("DEBUG Message <Exception node converted to list>:")
      print(layout)
    }
    
    l2 <- data.frame(Code = l1$Code, ErrorMessage = substr(l1$Message, 1, 70))
    l2
  }
  else
  {
    l2 <- ""
    l2
  }
}
