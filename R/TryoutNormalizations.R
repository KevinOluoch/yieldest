tryOutNormalizations <- function() {
  data.path <- yieldest::system.file("inst/extdata/MaizeYieldHHdata.csv",
                                     package = "yieldest")
  df <- utils::read.csv(data.path, stringsAsFactors = TRUE)

  targetV <- "maize_qua_kh"

  #   # All normalizations
  # dataoption1a <- data3
  print('All normalizations')
  a1 <- dataoption1_ts$dataoption1a[[1]]$AICofIV
  a1_ordered <- a1[order(a1[, "AIC"]),]
  print(a1_ordered[1:7,1:3])
  print(a1_ordered[1:3, "parameters"])
  # print(pvalues(df[, a1_ordered[1, "parameters"]], targetV))

  # All normalizations except target variable
  # dataoption1b <- data303
  #
  print('All normalizations except target variable')
  b1 <- dataoption1_ts$dataoption1b[[1]]$AICofIV
  b1_ordered <- b1[order(b1[, "AIC"]),]
  print(b1_ordered[1:7,1:3])
  print(a1_ordered[1:3, "parameters"])


  # # All normalizations but just square nomCols.numeric.skew
  # dataoption2a <- yieldest::normalization(data303, cols = nomCols.numeric.skew,
  #                                         method = "squares")

  print('All normalizations but just square nomCols.numeric.skew')
  a2 <- dataoption2_ts$dataoption2a[[1]]$AICofIV
  a2_ordered <- a2[order(a2[, "AIC"]),]
  print(a2_ordered[1:7,1:3])
  print(a1_ordered[1:3, "parameters"])

  # # All normalizations but standardize nomCols.numeric.skew
  # dataoption2b <- yieldest::normalization(data303, cols = nomCols.numeric.skew)
  #
  print('All normalizations but standardize nomCols.numeric.skew')
  b2 <- dataoption2_ts$dataoption2b[[1]]$AICofIV
  b2_ordered <- b2[order(b2[, "AIC"]),]
  print(b2_ordered[1:7,1:3])
  print(a1_ordered[1:3, "parameters"])


  #   dataoption3a <- data303[,colnames(data303)[!colnames(data303) %in% random.variables] ]
  print('No random variables')
  a3 <- dataoption3_ts$dataoption3a[[1]]$AICofIV
  a3_ordered <- a3[order(a3[, "AIC"]),]
  print(a3_ordered[1:7,1:3])
  print(a1_ordered[1:3, "parameters"])

  #
  #   dataoption3b <- data303[,colnames(data303)[!colnames(data303) %in% c("Admin_1", "year")] ]
  print('No random variables  aez is a fixed varizble')
  b3 <- dataoption3_ts$dataoption3b[[1]]$AICofIV
  b3_ordered <- b3[order(b3[, "AIC"]),]
  print(b3_ordered[1:7,1:3])
  print(a1_ordered[1:3, "parameters"])


  # Current model
  print("Current model")
  print(optimalModelData3$AICofIV[order(optimalModelData3$AICofIV[,1]),][1:7, 1:3])
  print(optimalModelData3$AICofIV[order(optimalModelData3$AICofIV[,1]),][1:7, "parameters"])


}
