#02_data_integration
# 01を実行したうえで、このスクリプトを開く
source("#01_data_cleansing.R") # まだ実行していない場合
ls() # オブジェクトの一覧を確認しておく

# 新しく作業を始めるので、番号を振りなおす
demo <- demo2
drug <- drug3
reac <- reac2
rm(list = ls()[!ls() %in% c("demo", "drug", "hist", "reac")])
gc(); gc(); #メモリの解放

# 1) 閲覧用・アプリ用の「患者情報テーブル」の編集
demo2 <- demo %>% 
    dplyr::left_join(
      .,
      drug %>%
        dplyr::group_by(識別番号) %>%
        dplyr::summarise(
          薬剤名 = paste(医薬品一般名, collapse = "\n"),
          薬剤数 = n(),
        ) %>%
        dplyr::mutate(
          多剤併用 = ifelse(薬剤数 >= 6, "多剤併用", "非多剤併用")
        )
    ) %>%
    dplyr::left_join(
      .,
      hist %>%
        dplyr::group_by(識別番号) %>%
        dplyr::summarise(適応症 = paste(原疾患等, collapse = "\n"))
    ) %>%
    dplyr::left_join(
      .,
      reac %>%
        dplyr::group_by(識別番号) %>%
        dplyr::summarise(
          有害事象 = paste(有害事象, collapse = "\n"),
          転帰 = paste(転帰, collapse = "\n")
        )
    ) %>%
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
    dplyr::select(
      識別番号, 薬剤名, 薬剤数, 多剤併用, 適応症, 有害事象, 転帰, 年齢, 年代, 体重, 性別, 身長,
      報告年度四半期
    ) %>%
    dplyr::mutate(
      薬剤名 = ifelse(is.na(薬剤名), "", 薬剤名),
      適応症 = ifelse(is.na(適応症), "", 適応症),
      有害事象 = ifelse(is.na(有害事象), "", 有害事象),
      転帰 = ifelse(is.na(転帰), "", 転帰)
    ) %>%
    # dplyr::mutate_all(as.factor) %>%
    print()

# "識別番号" 列と "性別" 列を選択
gender <- demo2 %>%
  select(識別番号, 性別)

# saveRDS(object = demo2, file = "appdata/demo.obj")

# 3) 閲覧用・アプリ用の「医薬品情報テーブル」の編集
drug2 <- drug %>%
  dplyr::select(
    識別番号, 医薬品一般名, 医薬品の関与, 経路, 投与単位, 分割投与回数, 
    使用理由,　医薬品の処置
  ) %>%
  dplyr::mutate(
    経路 = stringr::str_remove_all(
      string = 経路,
      pattern = "[:punct:]"
  )
  ) %>%
  dplyr::rename(薬剤名 = 医薬品一般名) %>%
  # dplyr::mutate_all(.funs = as.factor) %>%
  print()
# saveRDS(object = drug2, file = "appdata/drug.obj")

drug_summary <- drug2 %>%
  dplyr::group_by(薬剤名) %>%
  dplyr::summarise(
    件数 = n(), 
    経口 = sum(経路 == "経口"),
    静脈内点滴 = sum(経路 == "静脈内点滴"),
    皮下 = sum(経路 == "皮下"),
    経皮 = sum(経路 == "経皮"),
    吸入 = sum(経路 == "吸入"),
    その他不明 = sum(!経路 %in% c("経口", "静脈内点滴", "皮下", "経皮", "吸収"))
  ) %>%
  dplyr::arrange(-件数) %>%
  dplyr::filter(件数 >= 10) %>%
  print()

write.csv(x = drug_summary, file = "summary/drug_summary.csv", fileEncoding = "CP932", row.names = FALSE)

# 3) 閲覧用・アプリ用の「医薬品情報テーブル」の編集
reac2 <- reac %>%
  dplyr::select(識別番号, 有害事象, 転帰) %>%
  # dplyr::mutate_all(as.factor) %>%
  print()

saveRDS(object = reac2, file = "appdata/reac.obj")

reac_summary <- reac2 %>%
  dplyr::group_by(有害事象) %>%
  dplyr::summarise(
    件数 = n(), 
    高齢ROR = NA,
    多剤ROR = NA,
    死亡 = sum(転帰 == "死亡"),
    未回復 = sum(転帰 == "未回復"),
    後遺症あり = sum(転帰 == "後遺症あり"),
    軽快 = sum(転帰 == "軽快"),
    回復 = sum(転帰 == "回復"),
    その他不明 = sum(!転帰 %in% c("死亡", "未回復", "後遺症あり", "軽快", "回復"))
  ) %>%
  dplyr::arrange(-件数) %>%
  dplyr::filter(件数 >= 10) %>%
  print()

for (i in 1:nrow(reac_summary)) {
  ae <- as.character(reac_summary$有害事象[i])
  発症 <- stringr::str_detect(string = demo2$有害事象, pattern = ae)
  t1 <- table(発症, 高齢 = demo2$年代 == "高齢")
  ror1 <- round((t1[2,2] / t1[1,2]) / (t1[2,1] / t1[1,1]),3)
  reac_summary$高齢ROR[i] <- ror1
  reac_summary$高齢ROR_Lower[i] = ror1 * exp(-1.96 * sqrt(sum(1 / t1)))
  reac_summary$高齢ROR_Upper[i] = ror1 * exp(1.96 * sqrt(sum(1 / t1)))
  t2 <- table(発症, 多剤 = demo2$多剤併用 == "多剤併用")
  ror2 <- round((t2[2,2] / t2[1,2]) / (t2[2,1] / t2[1,1]),3)
  reac_summary$多剤ROR[i] <- ror2
  reac_summary$多剤ROR_Lower[i] = ror2 * exp(-1.96 * sqrt(sum(1 / t2)))
  reac_summary$多剤ROR_Upper[i] = ror2 * exp(1.96 * sqrt(sum(1 / t2)))
  cat(paste0(i, " / ", nrow(reac_summary), "   ", ae), fill = TRUE)
}

reac_summary2 <- reac_summary %>%
  dplyr::mutate(
      クラス = case_when(
          高齢ROR_Lower  > 1 & 多剤ROR_Lower  > 1 ~ "class1",
          高齢ROR_Lower  > 1 & 多剤ROR_Lower  <= 1 ~ "class2",
          高齢ROR_Lower  <= 1 & 多剤ROR_Lower  > 1 ~ "class3",
          高齢ROR_Lower  <= 1 & 多剤ROR_Lower  <= 1 ~ "class4"
      )
  ) %>%
  dplyr::relocate(クラス, .after = 件数)
view(reac_summary2)

write.csv(x = reac_summary2, file = "summary/reac_summary.csv", fileEncoding = "CP932", row.names = FALSE)

