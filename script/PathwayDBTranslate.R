mapping = read.csv("D://HMDB_V4_XML.csv")
pathwayDB = read.csv("D://Pathway.Database_Curated.csv")


keggMapping = list()
pubchemMapping = list()
chebiMapping = list()
for (i in 1:nrow(mapping)) {
  hmdbid = mapping[["HMDB"]][i]
  
  keggid = mapping[["KEGG"]][i]
  if (keggid != "") {
    keggMapping[keggid] = hmdbid
  }
  
  pubchemid = mapping[["Pubchem"]][i]
  if (pubchemid != "") {
    pubchemMapping[pubchemid] = hmdbid
  }
  
  chebiid = mapping[["CHEBI"]][i]
  if (chebiid != "") {
    chebiMapping[[chebiid]] = hmdbid
  }
}

SetCompositionName = c()
FoundId = c()
NotFoundId = c()
for (i in 1:nrow(pathwayDB)) {
  ids = unlist(strsplit(pathwayDB[["Set.Composition"]][i], split = ","))
  namelist = ""
  foundlist = ""
  notfoundlist = ""
  for (id in ids) {
    name = ""
    if (!is.null(keggMapping[[id]])) {
      name = keggMapping[[id]]
    }
    if (!is.null(pubchemMapping[[id]])) {
      name = pubchemMapping[[id]]
    }
    if (!is.null(chebiMapping[[id]])) {
      name = chebiMapping[[id]]
    }
    if (name != "") {
      if (namelist == "") {
        namelist = name
        foundlist = id
      } else {
        namelist = paste(namelist, name, sep = ", ")
        foundlist = paste(foundlist, id, sep = ", ")
      }
    } 
    # not found
    else {
      if (notfoundlist == "") {
        notfoundlist = id
      } else {
        notfoundlist = paste(notfoundlist, id, sep = ", ")
      }
    }
  }
  SetCompositionName = append(SetCompositionName, namelist)
  FoundId = append(FoundId, foundlist)
  NotFoundId = append(NotFoundId, notfoundlist)
}
pathwayDB[["Set.Composition"]] = SetCompositionName
pathwayDB[["FoundId"]] = FoundId
pathwayDB[["NotFoundId"]] = NotFoundId
write.csv(pathwayDB, "D://Pathway.Database.csv", row.names = FALSE)

write.csv(t(mapping[c("HMDB", "Name")]), "D://hmdb.csv")






####################
# 2023.07.10
####################
library(stringr)

data <- read.csv("D://Pathway.Database-HMDB-KEGG.csv")
setIdList = c()
setNameList = c()
idList = c()
found = c()
for (i in 1:nrow(data)) {
  setId = data[['Set.ID']][i]
  setName = data[['Set.Name']][i]
  foundId = data[['FoundId']][i]
  notFoundId = data[['NotFoundId']][i]
  currSetIdList = c()
  currSetNameList = c()
  currIdList = c()
  foundOrNot = c()
  for (id in unlist(strsplit(foundId, ','))) {
    currSetIdList = append(currSetIdList, setId)
    currSetNameList = append(currSetNameList, setName)
    currIdList = append(currIdList, str_trim(id))
    foundOrNot = append(foundOrNot, 'Found')
  }
  for (id in unlist(strsplit(notFoundId, ','))) {
    currSetIdList = append(currSetIdList, setId)
    currSetNameList = append(currSetNameList, setName)
    currIdList = append(currIdList, str_trim(id))
    foundOrNot = append(foundOrNot, 'NotFound')
  }
  setIdList = append(setIdList, currSetIdList)
  setNameList = append(setNameList, currSetNameList)
  idList = append(idList, currIdList)
  found = append(found, foundOrNot)
}
result = data.frame('Set.ID' = setIdList, 'Set.Name' = setNameList, 'Composition' = idList, 'FoundOrNot' = found)
write.csv(result, "D://result-2.csv")









