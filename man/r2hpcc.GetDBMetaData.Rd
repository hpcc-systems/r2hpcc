% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetDBMetaData.R
\name{r2hpcc.GetDBMetaData}
\alias{r2hpcc.GetDBMetaData}
\title{This methods allows you to query the HPCC Platform and get metadata to use to create a view or model of the target
HPCC system as a SQL accessible DB.}
\usage{
r2hpcc.GetDBMetaData(conn, includeTables = 1, tableFilter = "",
  includeStoredProcedures = 1, querySet = "", includeTargetClusters = 1,
  clusterType = "")
}
\arguments{
\item{conn}{- HPCC connection information}

\item{includeTables}{- If set to 1 or true, available tables are included in response}

\item{tableFilter}{- Filter for table results}

\item{includeStoredProcedures}{- If set to 1 or true, available Stored Procedures are included in response}

\item{querySet}{- QuerySet to use as filter for Stored procedures to return}

\item{includeTargetClusters}{- If set to 1 or true, available Target Clusters are included in response}

\item{clusterType}{- Cluster type to use as filter}
}
\value{
HPCC platform metadata
}
\description{
This methods allows you to query the HPCC Platform and get metadata to use to create a view or model of the target
HPCC system as a SQL accessible DB.
}
