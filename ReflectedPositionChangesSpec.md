# Introduction #
Volatility Skewの水平（＋時間）移動を、IVとオプション価格に反映させる仕様に関する考察

# Details #
  * SkewModelを利用してIV(IV\_Orig)を再計算するとき、新たなIVは回帰モデル上の曲線上に戻るという仕様になっている。
  * これは、Optionの割高、割安さが新しいIV\_OrigとそのOption価格が計算されるとき、反映されるということを前提とした仕様である。
  * 他の方法としては以下の分だけIV\_Origが変化するという仕様も考えられる。
> IV\_Orig\_after=
> IV\_Orig\_before x
> > SKewModel(Normalized\_skew\_after)/SKewModel(Normalized\_skew\_before)
  * Skewの水平移動分だけ、IV\_Origは変化するが、その割高さ、割安さは解消
されないとする仕様である。