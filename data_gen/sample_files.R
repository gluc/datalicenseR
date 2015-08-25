fileName <- "inst/extdata/getdata.out"
content <- readChar(fileName, file.info(fileName)$size)
getDataReply <- GetDataParser(content)
save(getDataReply, file = "data/getDataReply.rda", compress = "gzip")


fileName <- "inst/extdata/gethistory.out"
content <- readChar(fileName, file.info(fileName)$size)
getHistoryReply <- GetHistoryParser(content)
save(getHistoryReply, file = "data/getHistoryReply.rda", compress = "gzip")



fileName <- "inst/extdata/gethistory.out"
content <- readChar(fileName, file.info(fileName)$size)
getHistoryListReply <- GetHistoryListParser(content)
save(getHistoryListReply, file = "data/getHistoryListReply.rda", compress = "gzip")


fileName <- "inst/extdata/getsnap.out"
content <- readChar(fileName, file.info(fileName)$size)
getSnapshotReply <- GetSnapshotReplyParser(content)
save(getSnapshotReply, file = "data/getSnapshotReply.rda", compress = "gzip")


fileName <- "inst/extdata/getsnap.resp"
content <- readChar(fileName, file.info(fileName)$size)
getSnapshotResponse <- GetSnapshotResponseParser(content)
save(getSnapshotResponse, file = "data/getSnapshotResponse.rda", compress = "gzip")

