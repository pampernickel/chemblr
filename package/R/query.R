# PDobay for AG Bourquin, KISPI
# pamela.dobay@kispi.uzh.ch

getSynonyms <- function(query) {
  # given a simple query (drug name), extract information regarding synonyms; 
  # eventually extracted information would include other tags; if molecule exists in the 
  # db, return ChEMBL ID and all synonyms
  if (!is.loaded("RCurl")){
    library("RCurl")
  }
  
  if (!is.loaded("XML")){
    library("XML")
  }
  
  url <- sprintf("https://www.ebi.ac.uk/chembl/api/data/molecule/search?q=%s", query)
  h <- getCurlHandle()
  d <- xmlInternalTreeParse(getURL(url, curl=h))
  as.character(unlist(xpathApply(d, "//synonyms", xmlValue))) -> syns
  as.character(unlist(xpathApply(d, "//syn_type", xmlValue))) -> synt
  as.character(unlist(xpathApply(d, "//molecule_chembl_id", xmlValue))) -> chemblid
  unique(chemblid) -> chemblid
  syns[which(synt %in% "FDA")] -> fda
  unique(toupper(syns)) -> all.names
  res <- list(chemblid, fda, all.names)
  names(res) <- c("chemblid", "fda", "synonyms")
  return(res)
}
