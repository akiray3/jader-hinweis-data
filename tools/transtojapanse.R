cat("[translatejapanse]英名を日本語名に置換",fill = TRUE)

transtojapanse <- function(x){
	dat <- read.table("tools/英名-和名20220904.txt", head = FALSE,sep = ",")
	sapply(X = x,
		   FUN = function(x){
			  	ifelse(test = any(dat$V1 == x),
			  			yes = dat[dat$V1 == x,"V2"],
			  			no = x
			  		)		
			  })
	#作成者：相澤
	#医薬品名　英名→日本語に変換する
	#英名はアルファベット５文字以上を対象とした
	#日本語訳はdeeplによる
	#後から編集しやすい様に英名ー和名のリストはtxtファイルで作成(workspace_A参照)
	#英名-和名20220904.txt　を編集することで日本語訳の変更、名前の追加が可能

	#2022/09/10 吉田改修
	# dat（和名-英名YYYYMMDD.txtを関数内でパス指定して読み込み仕様にした）
	# README.txtに、必ずJaderResearchをworking directoryに指定するよう書いた
	#
	# >>相澤さん
	# この関数のなかで"dat"という（他でも使いそうな）名前のオブジェクトを定義していますが、
	# このオブジェクトの有効範囲（スコープ）は、この関数が実行されている間だけなので、大丈夫です
	# この関数が実行し終わると、datというオブジェクトは消去されます
}

