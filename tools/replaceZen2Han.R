cat("[replaceZen2Han] 全角の英数字を半角に変換", fill = TRUE)
replaceZen2Han <- function(x) {
    zen <- c("０", "１", "２", "３", "４", "５", "６", "７", "８", "９",
             "Ａ", "Ｂ", "Ｃ", "Ｄ", "Ｅ", "Ｆ", "Ｇ", "Ｈ", "Ｉ", "Ｊ", "Ｋ",
             "Ｌ", "Ｍ", "Ｎ", "Ｏ", "Ｐ", "Ｑ", "Ｒ", "Ｓ", "Ｔ", "Ｕ", "Ｖ", 
             "Ｗ", "Ｘ", "Ｙ", "Ｚ", 
             "ａ", "ｂ", "ｃ", "ｄ", "ｅ", "ｆ", "ｇ", "ｈ", "ｉ", "ｊ", "ｋ", 
             "ｌ", "ｍ", "ｎ", "ｏ", "ｐ", "ｑ", "ｒ", "ｓ", "ｔ", "ｕ", "ｖ", 
             "ｗ", "ｘ", "ｙ", "ｚ", "（", "）")
    han <- c(0:9, LETTERS, letters, "(", ")")
    xSplit <- as.matrix(unlist(strsplit(x = x, split = "")))
    ySplit <- apply(
        X = xSplit,
        MARGIN = 1, 
        FUN = function(x){
            ifelse(
                test = any(zen == x), 
                yes = han[which(zen == x)], 
                no = x
            )
        }
    )
    y <- paste(ySplit, collapse = "")
    return(y)
    
    ##### replaceZen2Han(x)
    # Description: 全角の英数字を半角に変換する関数
    # Usage: 
    #	replaceZen2Han(x = "ＣＢＲ６００ＲＲは４気筒（水冷）エンジンを搭載")
    # Arguments: 
    #  x = 全角英数字が混入した（した可能性のある）文字列
    #    
    # Developer: 吉田
    #
    # Note: 
    #  記号は括弧のみ対応。必要に応じて増やす
}
