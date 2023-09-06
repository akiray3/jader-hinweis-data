#01_data_cleansing
rm(list = ls())
library("tidyverse") # tidyveseパッケージが必要

# 更新時はyyyymmを変更する
yyyymm <- "202306"
fnames <- c("demo", "drug", "reac", "hist")
names(fnames) <- fnames

# データの読み込み
org <- purrr::map(
    .x = fnames,
    .f = function(x){
      xpath <- yyyymm %>%
        paste0("pmdacasereport", ., "/", x, .,".csv")
      y <- file(xpath, encoding = "CP932") %>%
        read.csv() %>%
        dplyr::as_tibble()
      return(y)
    }
  )
attach(org)
names(drug) <- stringr::str_remove_all(
    string = names(drug),
    pattern = "[:punct:]"
  )
names(demo) <- stringr::str_remove_all(
    string = names(demo),
    pattern = "[:punct:]"
  )

# 1）「症例一覧テーブル」の「性別」と「年齢」は、分析に使用する基本的属性であるため、
# これらが欠損または「不明」なっていた識別番号（症例）をデータ全体（4テーブル全て）から除外した。
# 2）分析対象を自発報告に統一するため、同テーブルの「報告の種類」が
# 「（臨床）試験」「その他」「不明」となっていた識別番号をデータ全体から除外した。
sum(demo$性別 %in% c("", "不明")) # 性別不明
sum(demo$年齢 %in% c("", "不明")) # 年齢不明
sum(demo$報告の種類 %in% c("試験", "その他", "不明")) # 自発報告以外

demo2 <- demo %>%
  dplyr::filter(!性別 %in% c("", "不明")) %>%
  dplyr::filter(!年齢 %in% c("", "不明")) %>%
  dplyr::filter(報告の種類 == "自発報告") %>%
  print()

# 確認
nrow(demo); nrow(demo2)
table(demo$性別); table(demo2$性別)
table(demo$年齢); table(demo2$年齢)
table(demo$報告の種類); table(demo2$報告の種類)

ok_demo_id <- demo2$識別番号

# 3)「副作用情報テーブル」における「識別番号」「有害事象」「有害事象の発現日」が
# 全て一致する場合は、重複して入力された可能性があるため、重複する行を
# 「副作用情報テーブル」から除外した。
reac2 <- reac %>%
  dplyr::filter(識別番号 %in% ok_demo_id) %>%
  dplyr::mutate(
    有害事象の発現日 = case_when(
      nchar(有害事象の発現日) > 8 ~ substr(有害事象の発現日, start = 1, stop = 8),
      nchar(有害事象の発現日) < 8  ~ "XXXXXXXX",
      TRUE ~ 有害事象の発現日
    ),
    case_id = paste0(識別番号, "-", 有害事象, "-", 有害事象の発現日)
  ) %>%
  dplyr::distinct(case_id, .keep_all = TRUE) %>%
  print()

# 4）有害事象との関連可能性が低い医薬品を除外するため、「医薬品情報テーブル」において、
# 「投与開始日」よりも前に「有害事象の発現日」が示された場合には、当該医薬品を含む行全体を
# 「医薬品情報テーブル」から除外した。ただし、投与期間が不明または不正確な医薬品については、
# その数が多いため、除外しなかった。
drug2 <- drug %>%
  dplyr::filter(識別番号 %in% ok_demo_id) %>%
  dplyr::left_join(
    .,
    reac2 %>%
      dplyr::mutate_at("有害事象の発現日", as.numeric) %>%
      dplyr::arrange(-有害事象の発現日) %>%
      dplyr::distinct(識別番号, .keep_all = TRUE) %>%
      dplyr::select(識別番号, 有害事象の発現日)
  ) %>%
  dplyr::mutate(
    投与開始日 = case_when(
      nchar(投与開始日) > 8 ~ substr(投与開始日, start = 1, stop = 8),
      nchar(投与開始日) < 8  ~ "XXXXXXXX",
      TRUE ~ 投与開始日
    ),
    投与終了日 = case_when(
      nchar(投与終了日) > 8 ~ substr(投与終了日, start = 1, stop = 8),
      nchar(投与終了日) < 8  ~ "XXXXXXXX",
      TRUE ~ 投与終了日
    ),
    判定 = 有害事象の発現日 > as.numeric(投与開始日)
  ) %>%
  dplyr::filter(判定 == TRUE | is.na(判定)) %>%
  dplyr::select(-判定) %>%
  dplyr::group_by(識別番号) %>%
  dplyr::distinct(医薬品一般名, .keep_all = TRUE) %>%
  print()

# 確認
nrow(drug); nrow(drug2)
length(unique(drug$医薬品一般名)); length(unique(drug2$医薬品一般名))

# 5）「医薬品情報テーブル」の「医薬品（一般名）」において、
# 一般名に含まれる半角文字を全角文字に統一し、不要な空白・記号を削除した。
# 6）同箇所において、英名で入力された一般名を、
# 国立医薬品食品衛生研究所「日本医薬品一般的名称データベース（JAN）を参照して、和名に統一した。
# 7）同箇所において、「一般薬」「外用薬」「カプセル」「フィルム」等の曖昧に記録された
# 一般名を「NA」に置換した。
source("tools/transtojapanse.R")
source("tools/replaceZen2Han.R")
drug3 <- drug2
ippanName <- as.factor(drug3$医薬品一般名)
old <- levels(ippanName)
cat(paste0("現在の一般名は", length(old), "種あります"), fill = TRUE)
tmp1 <- unlist(lapply(X = old, FUN = replaceZen2Han))
tmp1 <- toupper(tmp1)
cat(paste0(sum(old != tmp1), "件の全角英数字を、半角大文字に変換しました"), fill = TRUE)
tmp2 <- gsub(pattern = "[[:punct:]]| |　", replacement = "", x = tmp1)
cat(paste0(sum(tmp2 != tmp1), "件の空白・記号を削除しました"), fill = TRUE)
tmp3 <- transtojapanse(tmp2)
cat(paste0(sum(tmp3!= tmp2),"件の英名を日本語名に変換しました"), fill = TRUE)
tmp4 <- gsub(pattern = "一般薬",replacement = "", tmp3)
cat(paste0(sum(tmp4 != tmp3),"件の「一般薬」を削除しました"), fill = TRUE)
tmp5 <- gsub(pattern = "後続",replacement = "", tmp4)
cat(paste0(sum(tmp5 != tmp4),"件の「後続」を削除しました"), fill = TRUE)
tmp6 <- gsub(pattern = "酸化MG",replacement = "酸化マグネシウム", x = tmp5)
cat(paste0(sum(tmp6 !=tmp5),"件の「酸化MG」を「酸化マグネシウム」に変換しました"), fill = TRUE)
tmp7 <- gsub(pattern = paste0("[0-9][0-9][0-9]","MG"),replacement = "", x = tmp6)
tmp8 <- gsub(pattern = paste0("[0-9][0-9]","MG"),replacement = "", x = tmp7)
tmp9 <- gsub(pattern = paste0("[0-9]","MG"),replacement = "", x = tmp8)
cat(paste0(sum(tmp9 != tmp6),"件の規格を削除しました"), fill = TRUE)
tmp10 <- gsub(pattern = "カプセル",replacement = "", x = tmp9)
tmp11 <- gsub(pattern = "フィルム",replacement = "", x = tmp10)
tmp12 <- gsub(pattern = "ゼリー",replacement = "", x = tmp11)
tmp13 <- gsub(pattern = "細粒",replacement = "", x = tmp12)
tmp14 <- gsub(pattern = "注射剤",replacement = "", x = tmp13)
tmp15 <- gsub(pattern = "外用薬",replacement = "", x = tmp14)
tmp16 <- gsub(pattern = "外用剤",replacement = "", x = tmp15)
cat(paste0(sum(tmp16 != tmp9),"件の剤形を削除しました"), fill = TRUE)
tmp17 <- gsub(pattern = "塩酸塩",replacement = "", x = tmp16)
tmp18 <- gsub(pattern = "塩酸",replacement = "", x = tmp17)
tmp19 <- gsub(pattern = "シュウ酸塩",replacement = "", x = tmp18)
tmp20 <- gsub(pattern = "ナトリウム水和物炭酸水素ナトリウム",replacement = "", x = tmp19)
tmp21 <- gsub(pattern = "ナトリウム水和物Lグルタミン",replacement = "", x = tmp20)
tmp22 <- gsub(pattern = "ナトリウムLグルタミン",replacement = "", x = tmp21)
tmp23 <- gsub(pattern = "ナトリウム水和物",replacement = "", x = tmp22)
tmp24 <- gsub(pattern = "ナトリウム水和物",replacement = "", x = tmp23)
tmp25 <- gsub(pattern = paste0("遺伝子組換え","[1-3]"), replacement = "遺伝子組み換え", x = tmp24)
tmp26 <- gsub(pattern = "遺伝子組換え注", replacement = "遺伝子組み換え", x = tmp25)
tmp27 <- gsub(pattern = "安息香酸エステル", replacement = "", x = tmp26)
tmp28 <- gsub(pattern = paste0("配合剤", "[0-9]"), replacement = "配合剤", x = tmp27)
tmp29 <- gsub(pattern = paste0("酒石酸", "[1-3]"), replacement = "", x = tmp28)
cat(paste0(sum(tmp29 != tmp16), "件の酸塩を削除しました"), fill = TRUE)
tmp30 <- gsub(pattern = "去たん", replacement = "去痰", x = tmp29)
tmp31 <- gsub(pattern = "プロッカー", replacement = "ブロッカー", x = tmp30)
tmp32 <- gsub(pattern = "シクロホスファマイド", replacement = "シクロシクロホスファミド", x = tmp31)
cat(paste0(sum(tmp32 != tmp29), "件の表記揺れを修正しました"), fill = TRUE)
fixTable <- data.frame(old = old, new = tmp32)
levels(ippanName) <- tmp32
drug3$医薬品一般名 <- as.character(ippanName)
length(unique(drug3$医薬品一般名))

# 確認
nrow(drug); nrow(drug3)
nrow(reac); nrow(reac2)
length(unique(drug$医薬品一般名))
length(unique(drug3$医薬品一般名))

# 外部ファイルに書き出し
write.csv(x = demo2, file = "#01_cleansed_data/demo.csv", fileEncoding = "CP932", row.names = FALSE)
write.csv(x = drug3, file = "#01_cleansed_data/drug.csv", fileEncoding = "CP932", row.names = FALSE)
write.csv(x = reac2, file = "#01_cleansed_data/reac.csv", fileEncoding = "CP932", row.names = FALSE)
write.csv(x = hist, file = "#01_cleansed_data/hist.csv", fileEncoding = "CP932", row.names = FALSE)