bdlFields <- read.csv(file = "data_gen/fields.csv", stringsAsFactors = FALSE)
colnames(bdlFields) <- c(colnames(bdlFields)[-1], 'Field.ID')
bdlFields$Field.ID <- rownames(bdlFields)
rownames(bdlFields) <- bdlFields$Field.Mnemonic
#definition
bdlFields <- bdlFields[,c(-5, -19, -20, -21, -28)]


save(bdlFields, file = "R/sysdata.rda", compress = "gzip")
