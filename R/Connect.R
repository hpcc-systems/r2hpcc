#' Creates a connection object for HPCC
#'
#' Takes in the HPCC host name, target cluster, user name, password, reult limit
#' and returns a connection object
#'
#' @param host - ip address of HPCC host
#' @param targetcluster - Target Thor cluster
#' @param userid - user id - if the cluster is secure
#' @param password - password associated with the user
#' @param resultLimit - limits the result at connection level
#' @export
r2hpcc.Connect <- function(host, targetCluster, userId, password, resultLimit, debugMode = FALSE)
{
	suppressMessages(library(RCurl))
	suppressMessages(library(XML))
	hpccPassword <- password  
	hpccPassword <- gsub("\\!", "%21", hpccPassword)
	hpccPassword <- gsub("\\#", "%23", hpccPassword)
	hpccPassword <- gsub("\\$", "%24", hpccPassword)
	hpccPassword <- gsub("\\&", "%26", hpccPassword)
	hpccPassword <- gsub("\\'", "%27", hpccPassword)
	hpccPassword <- gsub("\\(", "%28", hpccPassword)
	hpccPassword <- gsub("\\)", "%29", hpccPassword)
	hpccPassword <- gsub("\\*", "%2A", hpccPassword)
	hpccPassword <- gsub("\\+", "%2B", hpccPassword)
	hpccPassword <- gsub("\\,", "%2C", hpccPassword)
	hpccPassword <- gsub("\\/", "%2F", hpccPassword)
	hpccPassword <- gsub("\\:", "%3A", hpccPassword)
	hpccPassword <- gsub("\\;", "%3B", hpccPassword)
	hpccPassword <- gsub("\\=", "%3D", hpccPassword)
	hpccPassword <- gsub("\\?", "%3F", hpccPassword)
	hpccPassword <- gsub("\\@", "%40", hpccPassword)
	hpccPassword <- gsub("\\[", "%5B", hpccPassword)
	hpccPassword <- gsub("\\]", "%5D", hpccPassword)
	connectionList <- c(host, targetCluster, userId, password, resultLimit, debugMode)
	connectionList
}