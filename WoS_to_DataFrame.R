# ----------------------------
# パッケージ
# ----------------------------

library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(DT)
library(rvest)
library(ggplot2)
library(data.table)


# ----------------------------
# 前処理
# ----------------------------


# そのままではうまくRにインポートできなかったため、以下の操作を実行
# ①Web of Scienceで論文検索したのち、詳細表示⇨タブ区切り（Mac）⇨ダウンロード
# ②キロオーサー論文は削除（Excel上で様式が大きくずれるファイル）
# ③Excelを用いてcsv変換
# ④WoSは500件ずつしかインポートできないため、繰り返しダウンロード
# ⑤各ファイルの保存先は「data」フォルダ


# ----------------------------------
# データのインポート
# ----------------------------------

# 「data」フォルダに格納されているファイル名を取得
files <- list.files("data", full.names = T)

# インポートと結合
d <- map(files, fread) %>% bind_rows 

# WoSのID以外の情報はネスト
dat <- d %>% select(PT, AU, AF, TI, SO, DT, CT, C1, RP, SN, EI, PY, DI, WC, SC, UT) %>%
    nest(data = setdiff(names(.), "UT"))


# ----------------------------------
# 関数作成
# ----------------------------------


clean.df <- function(x, y) {
    
    data <- x
    
    # 著者名の整理
    auth_full<- data$AF %>% str_split("; ") %>% unlist
    auth_abbr <- data$AU %>% str_split("; ") %>% unlist
    auth_df <- data.frame(auth_full, auth_abbr) %>%
        mutate(auth_full = as.character(auth_full)) %>%
        mutate(auth_abbr = as.character(auth_abbr))
    
    
    # 筆頭著者チェック
    auth_df <- auth_df %>% mutate(auth_first = c(1, rep(0, nrow(auth_df)-1))) 
    
    # 責任著者チェック
    RP <- data$RP %>% str_split("; ") %>% unlist %>%  # 責任著者ベクトルの作成
        str_extract("^.*?\\)") %>%                    # 文頭から ) まで
        str_replace(" \\(.*?\\)", "")                 # () の削除
    
    if(nrow(auth_df) == 1){             # 著者が１名の場合は自動的に責任著者
        
        auth_df$auth_rep <- 1           
        
    } else {                            # 著者が複数の場合は責任著者ベクトルに含まれるものをチェック
        auth_df <- auth_df %>% mutate(auth_rep = ifelse(auth_abbr %in% RP, 1, 0))
    }
    
    
    # 所属機関の整理
    if(!(str_detect(data$C1, "\\[.*?\\]"))){
        
        df <- auth_df %>% mutate(affi = paste0("Check: ", data$C1))
        
    } else {
        
        # 同じ所属の著者グループの抽出
        auth_group <- data$C1 %>% 
            str_extract_all("\\[.*?\\]") %>%    
            unlist %>%                          # ベクトル化
            str_replace_all("\\[", "") %>%      #  [ の削除
            str_replace_all("\\]", "")          #  ] の削除 
        
        # 所属を分割    
        affi <- data$C1 %>% 
            str_extract_all("\\[.*?(?=\\[|$)") %>%
            unlist %>%
            str_replace_all("\\[.*?\\]", "") %>%
            str_replace_all("; $", "") %>%
            str_trim(side = "both")
        
        # 著者グループと所属の統合、展開
        affi_df <- data.frame(auth_group, affi) %>%
            mutate(auth_full = str_split(auth_group, "; ")) %>%    # 著者グループを著者個人に分割（リスト）
            unnest(cols = auth_full) %>% 
            mutate(affi = str_split(affi, "; ")) %>%    # 著者グループを著者個人に分割（リスト）
            unnest(cols = affi)
        
        # 著者テーブルとの統合
        
        df <- auth_df %>% left_join(affi_df %>% select(-auth_group), by = c("auth_full" = "auth_full"))
    }
    
    # データの整理
    df <- df %>%
        mutate(pub.type = data$PT, pub.name = data$SO, doc.type = data$DT, doc.title = data$TI, conf.name = data$CT, issn = data$SN, e.issn = data$EI, 
               year = data$PY, doi = data$DI, wos.category = data$WC, res.area = data$SC, wos.num = y)
    
    
    # Warning: Reprint AuthorのAbbrと同じ著者がいた場合の措置（1ならばReprint Authorと同じAbbrの人がいる）
    auth_dup <- auth_df %>% group_by(auth_abbr) %>% summarize(N = n()) %>% filter(N > 1) %>% .$auth_abbr
    df <- df %>% mutate(rep.check = ifelse(auth_rep == 0, 0, 
                                           ifelse(auth_abbr %in% auth_dup, 1, 0)))
    
    # 出力
    df
}


# ---------------------
# 関数適用
# ---------------------

# 新しく作成したclean列に関数を適用した後のdfを格納

dat$clean <- map2(dat$data, dat$UT, clean.df)



# ---------------------
# 出力
# ---------------------

dat$clean %>% 
    bind_rows %>%
    write.csv("cleaned_df.csv")





