#02_data_integration
# 01を実行したうえで、このスクリプトを開く
# source("#01_data_cleansing.R") # まだ実行していない場合
ls() # オブジェクトの一覧を確認しておく

# 新しく作業を始めるので、番号を振りなおす
demo <- demo2
drug <- drug2 #3 10/2より、一旦drug3をdrug2に変更し、demo2の年齢のところも変えた
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
#          薬剤数 = length(unique(医薬品一般名)) 
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
saveRDS(object = drug2, file = "appdata/drug.obj")

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
#saveRDS(object = reac2, file = "appdata/reac.obj")


# "識別番号" 列と "性別" 列を選択 「GSP = gender_senior_polypharmacy」
GSP <- demo2 %>%　　
  select(識別番号, 性別, 年代, 多剤併用)%>% 
  mutate(高齢 = ifelse(年代 == "高齢", 1, 0))%>%
  mutate(多剤 = ifelse(多剤併用 == "多剤併用", 1, 0))

#reac2とGSPを結合させる
reac3 <- merge(reac2 , GSP , by = "識別番号" , all.x = T)



#===================================
#===================================

reac4M <- reac3 %>%
  dplyr::filter(性別 == "男性")%>%
  dplyr::group_by(有害事象) %>%
  #dplyr::group_by(有害事象 , 性別, 年代, 多剤併用) %>%
  dplyr::summarise(
    件数 = n(), 
    高齢ROR = NA,
    多剤ROR = NA,
    高齢LOW = NA,
    高齢UP  = NA,
    多剤LOW = NA,
    多剤UP  = NA,
  ) %>%
  dplyr::arrange(-件数) %>%
  dplyr::filter(件数 >= 10) %>%
  print()

for(i in 1:nrow(reac4M)) {
  ae <- as.character(reac4M$有害事象[i])
  subidM <- reac3 %>%
    dplyr::filter(有害事象 == ae & 性別　== "男性") %>%
    dplyr::pull(識別番号)
  #全患者の識別番号　＋　ロジスティック回帰分析
  resM <- demo2 %>% 
    dplyr::filter(性別　== "男性") %>% 
    dplyr::mutate(
      発症 = 識別番号 %in% subidM,
      年代 = 年代 == "高齢",
      多剤併用　= 多剤併用 =="多剤併用"
    ) %>%
    dplyr::select(発症, 年代, 多剤併用) %>% 
    glm(発症 ~ 年代 + 多剤併用, data = ., family=binomial()) %>%
    summary()
  #オッズ比を計算
  odds_ratios <- exp(coef(resM))
  reac4M$高齢ROR[i] <- odds_ratios["年代TRUE", "Estimate"]
  reac4M$高齢LOW[i] <- exp(resM$coefficients["年代TRUE", "Estimate"] - 1.96 * resM$coefficients["年代TRUE", "Std. Error"])
  reac4M$高齢UP[i]  <- exp(resM$coefficients["年代TRUE", "Estimate"] + 1.96 * resM$coefficients["年代TRUE", "Std. Error"])
  reac4M$多剤ROR[i] <- odds_ratios["多剤併用TRUE", "Estimate"]
  reac4M$多剤LOW[i] <- exp(resM$coefficients["多剤併用TRUE", "Estimate"] - 1.96 * resM$coefficients["多剤併用TRUE", "Std. Error"])
  reac4M$多剤UP[i] <- exp(resM$coefficients["多剤併用TRUE", "Estimate"] + 1.96 * resM$coefficients["多剤併用TRUE", "Std. Error"])
  cat(paste0(i, " / ", nrow(reac4M), "   ", ae), fill = TRUE)
}

reac4M2 <- reac4M %>%
  dplyr::mutate(
    クラス = case_when(
      reac4M$高齢LOW  > 1 & reac4M$多剤LOW  > 1 ~ "class1",
      reac4M$高齢LOW  > 1 & reac4M$多剤LOW  <= 1 ~ "class2",
      reac4M$高齢LOW  <= 1 & reac4M$多剤LOW  > 1 ~ "class3",
      reac4M$高齢LOW  <= 1 & reac4M$多剤LOW  <= 1 ~ "class4"
    )
  ) %>%
  dplyr::relocate(クラス, .after = 件数)


#===================================
#===================================



reac4F <- reac3 %>%
  dplyr::filter(性別 == "女性")%>%
  dplyr::group_by(有害事象) %>%
  #dplyr::group_by(有害事象 , 性別, 年代, 多剤併用) %>%
  dplyr::summarise(
    件数 = n(), 
    高齢ROR = NA,
    多剤ROR = NA,
    高齢LOW = NA,
    高齢UP  = NA,
    多剤LOW = NA,
    多剤UP  = NA,
  ) %>%
  dplyr::arrange(-件数) %>%
  dplyr::filter(件数 >= 10) %>%
  print()

for(i in 1:nrow(reac4F)) {
  ae <- as.character(reac4F$有害事象[i])
  subidF <- reac3 %>%
    dplyr::filter(有害事象 == ae & 性別　== "女性") %>%
    dplyr::pull(識別番号)
  #全患者の識別番号　＋　ロジスティック回帰分析
  resF <- demo2 %>% 
    dplyr::filter(性別　== "女性") %>% 
    dplyr::mutate(
      発症 = 識別番号 %in% subidF,
      年代 = 年代 == "高齢",
      多剤併用　= 多剤併用 =="多剤併用"
    ) %>%
    dplyr::select(発症, 年代, 多剤併用) %>% 
    glm(発症 ~ 年代 + 多剤併用, data = ., family=binomial()) %>%
    summary()
  #オッズ比を計算
  odds_ratios <- exp(coef(resF))
  reac4F$高齢ROR[i] <- odds_ratios["年代TRUE", "Estimate"]
  reac4F$高齢LOW[i] <- exp(resF$coefficients["年代TRUE", "Estimate"] - 1.96 * resF$coefficients["年代TRUE", "Std. Error"])
  reac4F$高齢UP[i]  <- exp(resF$coefficients["年代TRUE", "Estimate"] + 1.96 * resF$coefficients["年代TRUE", "Std. Error"])
  reac4F$多剤ROR[i] <- odds_ratios["多剤併用TRUE", "Estimate"]
  reac4F$多剤LOW[i] <- exp(resF$coefficients["多剤併用TRUE", "Estimate"] - 1.96 * resF$coefficients["多剤併用TRUE", "Std. Error"])
  reac4F$多剤UP[i] <- exp(resF$coefficients["多剤併用TRUE", "Estimate"] + 1.96 * resF$coefficients["多剤併用TRUE", "Std. Error"])
  cat(paste0(i, " / ", nrow(reac4F), "   ", ae), fill = TRUE)
}


reac4F2 <- reac4F %>%
  dplyr::mutate(
    クラス = case_when(
      reac4F$高齢LOW  > 1 & reac4F$多剤LOW  > 1 ~ "class1",
      reac4F$高齢LOW  > 1 & reac4F$多剤LOW  <= 1 ~ "class2",
      reac4F$高齢LOW  <= 1 & reac4F$多剤LOW  > 1 ~ "class3",
      reac4F$高齢LOW  <= 1 & reac4F$多剤LOW  <= 1 ~ "class4"
    )
  ) %>%
  dplyr::relocate(クラス, .after = 件数)

#===================================
#===================================

  
view(reac4F2)
view(reac4M2)

dir.create("summary", showWarnings = FALSE)
write.csv(x = reac4F2, file = "summary/201808Female_data1002.csv", fileEncoding = "CP932", row.names = FALSE)
write.csv(x = reac4M2, file = "summary/201808Male_data1002.csv", fileEncoding = "CP932", row.names = FALSE)
#write.csv(x = reac_summary2, file = "summary/reac_summary.csv", fileEncoding = "CP932", row.names = FALSE)
#write.csv(x = Female_data, file = "summary/201808Female_data.csv", fileEncoding = "CP932", row.names = FALSE)
#write.csv(x = Male_data, file = "summary/201808Male_data.csv", fileEncoding = "CP932", row.names = FALSE)

