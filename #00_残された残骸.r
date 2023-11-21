library(tidyverse)
library(ggrepel)
library(fst)
setwd("/Users/akira/GitHub/jader-hinweis/")
demo_0 <- readRDS(file = "/Users/akira/OneDrive - 城西大学/00_研究/2023_Jdr_a/pmdacasereport202206_demo.JD")
drug_0 <- readRDS(file = "/Users/akira/OneDrive - 城西大学/00_研究/2023_Jdr_a/pmdacasereport202206_drug.JD")
hist_0 <- readRDS(file = "/Users/akira/OneDrive - 城西大学/00_研究/2023_Jdr_a/pmdacasereport202206_hist.JD")
reac_0 <- readRDS(file = "/Users/akira/OneDrive - 城西大学/00_研究/2023_Jdr_a/pmdacasereport202206_reac.JD")

out <- list(全体 = NA, 男性 = NA, 女性 = NA) # 男女でループさせる
reacout <- list(全体 = NA, 男性 = NA, 女性 = NA) # 男女でループさせる
drugout <- list(全体 = NA, 男性 = NA, 女性 = NA) # 男女でループさせる

# seibetsu <- "女性" # テスト用
for(seibetsu in c("全体", "男性", "女性")) {
    # 要件1a: demoから、男性と女性の識別番号を抽出しておく
    if (seibetsu == "全体") {
        submitrow <- which(demo_0$報告の種類 != "試験")
        id_by_seibestu <- demo_0$識別番号[submitrow]
    } else {
        submitrow <- which(
            demo_0$性別  == seibetsu & demo_0$報告の種類 != "試験"
        )
        id_by_seibestu <- demo_0$識別番号[submitrow]
    }
    # 要件2準備: demoの疾患の「年齢」を高齢（70歳以上）と非高齢（70歳未満）に上書きする
    demo_1 <- demo_0 %>%
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

    reac_1 <- reac_0 %>%
        dplyr::as_tibble() %>%
        dplyr::filter(識別番号 %in% id_by_seibestu) %>%
        dplyr::mutate(case_id = paste0(識別番号, "-", 有害事象, "-", 有害事象の発現日)) %>%
        dplyr::relocate(case_id, .after = 識別番号) %>%
        # distinctでcase_idの重複行を削除
        dplyr::distinct(case_id, .keep_all = TRUE) %>%
        dplyr::left_join(x = ., y = demo_1)

    drug_1 <- drug_0 %>%
        dplyr::as_tibble() %>%
        dplyr::filter(識別番号 %in% id_by_seibestu) %>%
        dplyr::left_join(x = ., y = reac_1) %>%
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
        # 同一のcase_idで一般名が大量に重複しているので、distinctで重複除去
        dplyr::distinct(一般名, .keep_all = TRUE) %>%
        dplyr::select(識別番号, case_id, 年代, 年齢, 性別, 一般名, 有害事象, 転帰, everything())

    reac_2 <- drug_1 %>%
        dplyr::group_by(case_id) %>%
        dplyr::summarise(薬剤数 = n()) %>%
        dplyr::mutate(多剤併用 = ifelse(薬剤数 >= 6, "多剤", "非多剤")) %>%
        dplyr::left_join(reac_1, .) %>%
        dplyr::filter(case_id %in% drug_1$case_id) %>%
        dplyr::select(識別番号, case_id, 有害事象, 年代, 年齢, 性別, 多剤併用, 薬剤数, 転帰, everything())

    reacs_top200 <- reac_1 %>%
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
        tmpbyage <- reac_1 %>%
            dplyr::filter(有害事象 == reacs_top200$有害事象[i])
        tmpbyage_non <- reac_1 %>%
            dplyr::filter(有害事象 != reacs_top200$有害事象[i])
        reacs_top200$高齢_発症数[i] <- sum(tmpbyage$年代 == "高齢", na.rm = TRUE)
        reacs_top200$高齢_非発症数[i] <- sum(tmpbyage_non$年代 == "高齢", na.rm = TRUE)
        reacs_top200$非高齢_発症数[i] <- sum(tmpbyage$年代 == "非高齢", na.rm = TRUE)
        reacs_top200$非高齢_非発症数[i] <- sum(tmpbyage_non$年代 == "非高齢", na.rm = TRUE)
        tmpbymulti <- reac_2 %>%
            dplyr::filter(有害事象 == reacs_top200$有害事象[i])
        tmpbymulti_non <- reac_2 %>%
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
                    (1　/　高齢_発症数) + (1　/　高齢_非発症数) + 
                      (1　/　非高齢_発症数) + (1　/　非高齢_非発症数)
                  )
              ),
            高齢発症ROR_Upper = 高齢発症ROR *
              exp(
                1.96 * sqrt(
                    (1　/　高齢_発症数) + (1　/　高齢_非発症数) + 
                      (1　/　非高齢_発症数) + (1　/　非高齢_非発症数)
                  )
              ),
            多剤発症ROR = (多剤_発症数 / 多剤_非発症数) / (非多剤_発症数 / 非多剤_非発症数),
            多剤発症ROR_Lower = 多剤発症ROR *
              exp(
                -1.96 * sqrt(
                    (1　/　多剤_発症数) + (1　/　多剤_非発症数) + 
                      (1　/　非多剤_発症数) + (1　/　非多剤_非発症数)
                  )
              ),
            多剤発症ROR_Upper = 多剤発症ROR *
              exp(
                1.96 * sqrt(
                    (1　/　多剤_発症数) + (1　/　多剤_非発症数) + 
                      (1　/　非多剤_発症数) + (1　/　非多剤_非発症数)
                  )
              ),
        )
    out[[seibetsu]] <- reacs_top200
    if (seibetsu == "全体") {
      reacout <- reac_2
      drugout <- drug_1
    }
    cat(paste0(seibetsu, "の処理が完了"), fill = TRUE)
}

out2 <- rbind(out[[1]], out[[2]], out[[3]]) %>%
  dplyr::group_by(性別) %>%
  dplyr::mutate(
    順位 = row_number(),
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
  dplyr::select(識別番号, 一般名, 医薬品の関与, 年齢, 年代, 性別, 多剤併用, 有害事象, 転帰, 薬剤数) %>%
  dplyr::ungroup() %>%
  dplyr::select(-case_id)
detail$識別番号 <- as.factor(detail$識別番号)
detail$年齢 <- as.factor(detail$年齢)
detail$年代 <- as.factor(detail$年代)
detail$性別 <- as.factor(detail$性別)
detail$多剤併用 <- as.factor(detail$多剤併用)
detail$有害事象 <- as.factor(detail$有害事象)
detail$転帰 <- as.factor(detail$転帰)
detail$一般名 <- as.factor(detail$一般名)
detail$医薬品の関与 <- as.factor(detail$医薬品の関与)

# saveRDS(object = out2,  file = "tbl_200.obj")
# saveRDS(object = detail,  file = "tbl_all.obj")

fst::write.fst(x = out2, path = "tbl_200.obj")
fst::write.fst(x = detail, path = "tbl_all.obj")


##########################################################
out2 <- readRDS(file = "tbl_200.obj")
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

detail <- readRDS(file = "tbl_all.obj") %>%
  dplyr::mutate(識別番号 = as.character(識別番号))
saveRDS(object = detail,  file = "tbl_all.obj")
out2 <- readRDS(file = "tbl_200.obj")

needs(fst)
tbl_dtl <- fst::read.fst("tbl_all.obj") %>%
  dplyr::as_tibble()
test<-tbl_dtl %>%
  dplyr::filter(有害事象 == "発熱") %>%
  dplyr::group_by(一般名) %>%
  summarise(n = n()) %>%
  mutate(p = 100 * n/sum(n)) %>%
  dplyr::arrange(n)
view(test)




