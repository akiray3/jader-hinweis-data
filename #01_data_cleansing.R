#01_data_cleansing
rm(list = ls())
library("tidyverse") # tidyveseパッケージが必要
library("googledrive")
library("ggrepel")
library("fst")

# 更新時はyyyymmを変更する
drive_id <- "1yfJAV3PeOzGNMQh8I_By9LvjpWEUTyuD"

# このGoogleドライブ経由でデータをダウンロードする際に
# アカウントの認証が求められる。特に影響はないので、自身のアカウントを使用すると良い

# # 202306
googledrive::drive_download(file = as_id("15f_BnzcUhxivz4flGQnHjJkojp3q1DK6"), "demo.csv", overwrite = TRUE)
googledrive::drive_download(file = as_id("14l2V4M53uSUvx7DwfXMJEj0_1ZI_CNk6"), "drug.csv", overwrite = TRUE)
googledrive::drive_download(file = as_id("1y85i-CSibeHAR9NSao3a2bdHbcYHxMVq"), "reac.csv", overwrite = TRUE)
googledrive::drive_download(file = as_id("1u0S4YU_sx1QhtamaMRvYZ4HoFJnnEJYy"), "hist.csv", overwrite = TRUE)

# # 201808
# googledrive::drive_download(file = as_id("1MH_j8_pTbV_Gy8Gzewr8_TLDhsX2lXGh"), "demo.csv", overwrite = TRUE)
# googledrive::drive_download(file = as_id("1egizpavp7s6pGhGO1S48FBvqDc5nUgI_"), "drug.csv", overwrite = TRUE)
# googledrive::drive_download(file = as_id("16Q7IxdwRlzbCNBC7Pp4cCYg_p7Kv8nGB"), "reac.csv", overwrite = TRUE)
# googledrive::drive_download(file = as_id("1vkDx7u80w0gF5flLuiJh7prsUQfp5gux"), "hist.csv", overwrite = TRUE)


# データの読み込み
demo <- dplyr::as_tibble(read.csv(file("demo.csv", encoding = "CP932")))
drug <- dplyr::as_tibble(read.csv(file("drug.csv", encoding = "CP932")))
reac <- dplyr::as_tibble(read.csv(file("reac.csv", encoding = "CP932")))
hist <- dplyr::as_tibble(read.csv(file("hist.csv", encoding = "CP932")))
file.remove(paste0(c("demo", "drug", "reac", "hist"), ".csv"))

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

# 5）「医薬品情報テーブル」の「医薬品（医薬品一般名）」において、
# 医薬品一般名に含まれる半角文字を全角文字に統一し、不要な空白・記号を削除した。
# 6）同箇所において、英名で入力された医薬品一般名を、
# 国立医薬品食品衛生研究所「日本医薬品一般的名称データベース（JAN）を参照して、和名に統一した。
# 7）同箇所において、「一般薬」「外用薬」「カプセル」「フィルム」等の曖昧に記録された
# 医薬品一般名を「NA」に置換した。
source("tools/transtojapanse.R")
source("tools/replaceZen2Han.R")
drug3 <- drug2
ippanName <- as.factor(drug3$医薬品一般名)
old <- levels(ippanName)
cat(paste0("現在の医薬品一般名は", length(old), "種あります"), fill = TRUE)
tmp1 <- unlist(lapply(X = old, FUN = replaceZen2Han))
tmp1 <- toupper(tmp1)
cat(paste0(sum(old != tmp1), "件の全角英数字を、半角大文字に変換しました"), fill = TRUE)
tmp2 <- gsub(pattern = "[[:punct:]]| |", replacement = "", x = tmp1)
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

### 2023/11/21追加

out <- list(全体 = NA, 男性 = NA, 女性 = NA) # 男女でループさせる
reacout <- list(全体 = NA, 男性 = NA, 女性 = NA) # 男女でループさせる
drugout <- list(全体 = NA, 男性 = NA, 女性 = NA) # 男女でループさせる

# seibetsu <- "女性" # テスト用
for(seibetsu in c("全体", "男性", "女性")) {
    # 要件1a: demoから、男性と女性の識別番号を抽出しておく
    if (seibetsu == "全体") {
        submitrow <- which(demo2$報告の種類 != "試験")
        id_by_seibestu <- demo2$識別番号[submitrow]
    } else {
        submitrow <- which(
            demo2$性別  == seibetsu & demo2$報告の種類 != "試験"
        )
        id_by_seibestu <- demo2$識別番号[submitrow]
    }
    # 要件2準備: demoの疾患の「年齢」を高齢（70歳以上）と非高齢（70歳未満）に上書きする
    demo3 <- demo2 %>%
        dplyr::as_tibble() %>%
        dplyr::filter(識別番号 %in% id_by_seibestu) %>%
        dplyr::mutate(
            年代 = case_when(
                年齢 %in% c(
                    paste0(seq(from = 10, to = 60, by = 10), "歳代"), "10歳未満", 
                    "成人", "小児", "青少年", "新生児", "乳幼児",
                    "第三トライメスター", "第二トライメスター",
                    "第一トライメスター", "胎児"
                ) ~ "非高齢",
                年齢 %in% c(
                    paste0(seq(from = 70, to = 200, by = 10), "歳代"),
                    "高齢者"
                ) ~ "高齢"
            )
        ) %>%
        dplyr::select(識別番号, 年代, 年齢, 性別)

    reac3 <- reac2 %>%
        dplyr::as_tibble() %>%
        dplyr::filter(識別番号 %in% id_by_seibestu) %>%
        dplyr::mutate(case_id = paste0(識別番号, "-", 有害事象, "-", 有害事象の発現日)) %>%
        dplyr::relocate(case_id, .after = 識別番号) %>%
        # distinctでcase_idの重複行を削除
        dplyr::distinct(case_id, .keep_all = TRUE) %>%
        dplyr::left_join(x = ., y = demo3)

    drug4 <- drug3 %>%
        dplyr::as_tibble() %>%
        dplyr::filter(識別番号 %in% id_by_seibestu) %>%
        dplyr::mutate(有害事象の発現日 = as.character(有害事象の発現日)) %>%
        dplyr::select(-報告回数) %>%
        dplyr::left_join(x = ., y = reac3) %>%
        dplyr::mutate(
            有害事象の発現日 = as.numeric(有害事象の発現日),
            投与開始日 = as.numeric(投与開始日),
            投与終了日 = as.numeric(投与終了日),
            # 投与開始日が発現より後の時はerror1 = 1
            # 投与終了日が発現より前の時はerror2 = 1
            error1 = case_when(投与開始日 > 有害事象の発現日 ~ 1, TRUE ~ 0),
            error2 = case_when(投与終了日 < 有害事象の発現日 ~ 1, TRUE ~ 0),
            # error1とerror2の合計をerrorとし、0のみ取り出す
            error = error1 + error2
        ) %>%
        dplyr::filter(error == 0) %>%
        dplyr::select(-starts_with("error")) %>%
        dplyr::group_by(case_id) %>%
        # 同一のcase_idで医薬品一般名が大量に重複しているので、distinctで重複除去
        dplyr::distinct(医薬品一般名, .keep_all = TRUE) %>%
        dplyr::select(識別番号, case_id, 年代, 年齢, 性別, 医薬品一般名, 有害事象, 転帰, everything())

    reac4 <- drug4 %>%
        dplyr::group_by(case_id) %>%
        dplyr::summarise(薬剤数 = n()) %>%
        dplyr::mutate(多剤併用 = ifelse(薬剤数 >= 6, "多剤", "非多剤")) %>%
        dplyr::left_join(reac3, .) %>%
        dplyr::filter(case_id %in% drug4$case_id) %>%
        dplyr::select(識別番号, case_id, 有害事象, 年代, 年齢, 性別, 多剤併用, 薬剤数, 転帰, everything())

    reacs_top200 <- reac3 %>%
        dplyr::group_by(有害事象) %>%
        dplyr::summarise(件数 = n()) %>%
        dplyr::arrange(-件数) %>%
        dplyr::slice(1:200) %>%
        dplyr::mutate(
            性別 = seibetsu,
            高齢_発症数 = NA, 高齢_非発症数 = NA,
            非高齢_発症数 = NA, 非高齢_非発症数 = NA,
            多剤_発症数 = NA, 多剤_非発症数 = NA,
            非多剤_発症数 = NA, 非多剤_非発症数 = NA
        ) %>%
        dplyr::select(性別, everything())

    for (i in 1:200) {
        tmpbyage <- reac3 %>%
            dplyr::filter(有害事象 == reacs_top200$有害事象[i])
        tmpbyage_non <- reac3 %>%
            dplyr::filter(有害事象 != reacs_top200$有害事象[i])
        reacs_top200$高齢_発症数[i] <- sum(tmpbyage$年代 == "高齢", na.rm = TRUE)
        reacs_top200$高齢_非発症数[i] <- sum(tmpbyage_non$年代 == "高齢", na.rm = TRUE)
        reacs_top200$非高齢_発症数[i] <- sum(tmpbyage$年代 == "非高齢", na.rm = TRUE)
        reacs_top200$非高齢_非発症数[i] <- sum(tmpbyage_non$年代 == "非高齢", na.rm = TRUE)
        tmpbymulti <- reac4 %>%
            dplyr::filter(有害事象 == reacs_top200$有害事象[i])
        tmpbymulti_non <- reac4 %>%
            dplyr::filter(有害事象 != reacs_top200$有害事象[i])
        reacs_top200$多剤_発症数[i] <- sum(tmpbymulti$多剤併用 == "多剤", na.rm = TRUE)
        reacs_top200$多剤_非発症数[i] <- sum(tmpbymulti_non$多剤併用 == "多剤", na.rm = TRUE)
        reacs_top200$非多剤_発症数[i] <- sum(tmpbymulti$多剤併用 == "非多剤", na.rm = TRUE)
        reacs_top200$非多剤_非発症数[i] <- sum(tmpbymulti_non$多剤併用 == "非多剤", na.rm = TRUE)
    }

    # 要件3c: 多剤 x 発症のRORを計算する(多剤による増加程度)
    reacs_top200 <- reacs_top200 %>%
        dplyr::mutate(
            高齢発症ROR = (高齢_発症数 / 高齢_非発症数) / (非高齢_発症数 / 非高齢_非発症数),
            高齢発症ROR_Lower = 高齢発症ROR *
              exp(
                -1.96 * sqrt(
                    (1/高齢_発症数) + (1/高齢_非発症数) + 
                      (1/非高齢_発症数) + (1/非高齢_非発症数)
                  )
              ),
            高齢発症ROR_Upper = 高齢発症ROR *
              exp(
                1.96 * sqrt(
                    (1/高齢_発症数) + (1/高齢_非発症数) + 
                      (1/非高齢_発症数) + (1/非高齢_非発症数)
                  )
              ),
            多剤発症ROR = (多剤_発症数 / 多剤_非発症数) / (非多剤_発症数 / 非多剤_非発症数),
            多剤発症ROR_Lower = 多剤発症ROR *
              exp(
                -1.96 * sqrt(
                    (1/多剤_発症数) + (1/多剤_非発症数) + 
                      (1/非多剤_発症数) + (1/非多剤_非発症数)
                  )
              ),
            多剤発症ROR_Upper = 多剤発症ROR *
              exp(
                1.96 * sqrt(
                    (1/多剤_発症数) + (1/多剤_非発症数) + 
                      (1/非多剤_発症数) + (1/非多剤_非発症数)
                  )
              ),
        )
    out[[seibetsu]] <- reacs_top200
    if (seibetsu == "全体") {
      reacout <- reac4
      drugout <- drug4
    }
    cat(paste0(seibetsu, "の処理が完了"), fill = TRUE)
}

out2 <- rbind(out[[1]], out[[2]], out[[3]]) %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(性別) %>%
  dplyr::mutate(
    クラス = case_when(
      高齢発症ROR_Lower > 1 & 多剤発症ROR_Lower > 1 ~ "Class1_高齢・多剤",
      高齢発症ROR_Lower > 1 & 多剤発症ROR_Upper < 1 ~ "Class2_高齢・非多剤",
      高齢発症ROR_Upper < 1 & 多剤発症ROR_Lower > 1 ~ "Class3_非高齢・多剤",
      高齢発症ROR_Upper < 1 & 多剤発症ROR_Upper < 1 ~ "Class4_非高齢・非多剤",
      TRUE ~ "Class0_中間"
    )
  ) %>%
  dplyr::relocate(順位, .after = 性別) %>%
  dplyr::relocate(クラス, .after = 順位) %>%
  dplyr::mutate_at(
    .vars = vars(matches("ROR")),
    .funs = round, 2
  ) %>%
  dplyr::arrange() %>%
  dplyr::ungroup()

myclassset <- c("Class0_中間", "Class1_高齢・多剤", 
  "Class2_高齢・非多剤", "Class3_非高齢・多剤", 
  "Class4_非高齢・非多剤")
out2$性別 <- factor(out2$性別, c("全体", "男性", "女性"))
out2$クラス <- factor(out2$クラス, myclassset, myclassset)
out2$有害事象 <- as.factor(out2$有害事象)

detail <- reacout %>%
  dplyr::select(case_id, 多剤併用, 薬剤数) %>%
  dplyr::left_join(drugout, .) %>%
  dplyr::select(識別番号, 医薬品一般名, 医薬品の関与, 年齢, 年代, 性別, 多剤併用, 有害事象, 転帰, 薬剤数) %>%
  dplyr::ungroup() %>%
  dplyr::select(-case_id) %>% 
  dplyr::mutate(識別番号 = as.character(識別番号))

detail$識別番号 <- as.factor(detail$識別番号)
detail$年齢 <- as.factor(detail$年齢)
detail$年代 <- as.factor(detail$年代)
detail$性別 <- as.factor(detail$性別)
detail$多剤併用 <- as.factor(detail$多剤併用)
detail$有害事象 <- as.factor(detail$有害事象)
detail$転帰 <- as.factor(detail$転帰)
detail$医薬品一般名 <- as.factor(detail$医薬品一般名)
detail$医薬品の関与 <- as.factor(detail$医薬品の関与)

saveRDS(object = out2,  file = "tbl_200.obj")
saveRDS(object = detail,  file = "tbl_all.obj")

figlist <- list(全体 = NULL, 男性 = NULL, 女性 = NULL)
for (seibetsu in c("全体", "男性", "女性")){
  tmp <- out2 %>%
    dplyr::filter(性別 == seibetsu)
  sizerange <- as.vector(summary(tmp$件数)[c(2,3,5)])
  figlist[[seibetsu]] <- tmp %>%
    ggplot2::ggplot(
      mapping = aes(
        x = 多剤発症ROR,
        y = 高齢発症ROR,
        size = 件数,
        color = クラス
      )
    ) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::guides(color = guide_legend(override.aes = list(size = 10))) +
    ggplot2::scale_size(range = c(1, 25), guide = NULL) +
    ggplot2::scale_x_log10(breaks = seq(0, 20, 0.5)) +
    ggplot2::scale_y_log10(breaks = seq(0, 20, 0.5)) +
    ggplot2::scale_color_manual(
      values = c("#b2bec3", "#6c5ce7", "#0984e3", "#00cec9", "#00b894")
    ) +
    ggplot2::geom_vline(xintercept = 1) +
    ggplot2::geom_hline(yintercept = 1) +
    ggplot2::theme_gray(
      base_size = 14,
      base_family = "HiraKakuProN-W6" # Windowsではコメントアウト
    ) +
    ggplot2::labs(caption = paste0("データ: ", seibetsu)) + 
    ggplot2::theme(legend.position = "bottom") + 
    ggplot2::guides(color = guide_legend(nrow = 2, byrow = TRUE))
}
saveRDS(object = figlist,  file = "fig_list.obj")



