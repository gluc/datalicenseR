fileName <- "tests/data/sample1.out"
content <- readChar(fileName, file.info(fileName)$size)
response1 <- ParseBdlResponse(content)
save(response1, file = "data/response1.rda", compress = "gzip")
