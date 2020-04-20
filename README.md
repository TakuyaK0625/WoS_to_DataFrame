# WoS_to_DataFrame
Web of Scienceからダウンロードできるデータは論文情報が一行で表現されているため、研究者に焦点を当てた分析が行いづらいという課題がありました。このコードではWeb of Scienceからダウンロードできる論文情報を、研究者×所属機関単位に行展開し、著者単位、所属機関単位でのソートや集計を容易にします。また、このコードでは各著者が筆頭著者か否か、また、Reprint Authorか否かの情報も予め付与します。


## 使い方
1. 「data」フォルダにWeb of Scienceからダウンロードしたファイルを保管（＋ある程度のクリーニング）
2. WoS_to_DataFrame.Rを実行（※作業ディレクトリは同じディレクトリにしておく）


## 備考
私はRにデータを読み込むまでが大変でした。私が辿ったステップは以下の通りですが、お使いのOSやその他の環境によっては違うプロセスが必要かもしれません。何れにせよ、Rにデータを読み込ませるためにはある程度の下準備が必要そうです。

1. Web of Scienceで論文検索したのち、詳細表示⇨タブ区切り（Mac）⇨ダウンロード
2. キロオーサー論文は削除（Excel上で様式が大きくずれるファイル）
3. Excelを用いてcsv変換
