---
title: "スペクトル色の混合"
type: docs
---
Lumi のパレット システムは、スペクトル カラー モデルを使用して、実際の顔料がどのように混合するかをシミュレートします。目標は、デジタル パレットから色を構築して選択するエクスペリエンスを、物理的な絵の具を混ぜるかのように動作させることです。カラーがキャンバスに適用されると、それは標準の RGB になります。

## スペクトル混合の意味

従来の RGB 混合は加算的です。2 つの RGB 値をブレンドすると、中間点に向かって平均化されます。顔料の混合は減算法で行われます。各顔料は特定の波長を吸収し、それらを組み合わせた効果はより暗くなり、色相が変化することがよくあります。

Lumi models this using a 10-band spectral reflectance representation for palette colors, rather than RGB.

これにより、ペイントのような結果が得られます。青と黄色を混ぜると、グレーではなく緑が生成されます。 2 つの飽和した色を混合すると、物理的な顔料と同じようにニュートラルに移行する色が生成されます。

スペクトル計算は、パレットの構築中、つまり 2 番目と 3 番目のパレット エントリを生成するとき、およびパレット ミキサーが 2 つの親色をブレンドするときに実行されます。結果の色は、表示およびペイントのためにリニア RGB に変換されます。

## 顔料プロファイル

パレット エントリは、**カラー インデックス (CI) コード** を使用して実際の顔料データに基づくことができます。各 CI 顔料ファミリーには、混合方法に影響を与える特徴的なスペクトル バイアスがあります。

|顔料の役割 |混合動作 |例 |
| :--- | :--- | :--- |
| **プライマリ** |高彩度、きれいな二次 | PY3(レモンイエロー)、PR122(マゼンタ) |
| **本体** |不透明で強いマストーン、グリーンミックスではオリーブ色に変化 | PY35 (カドミウムイエロー)、PR108 (カドミウムレッド) |
| **中和剤** |急速に彩度を下げてミュートします。 PBk11 (マーズブラック)、PBr7 (シエナ) |
| **クロマアンカー** |高い着色力、混合物を支配します。 PB29（ウルトラマリンブルー）、PG7（フタログリーン） |

CI コードを含む原色をパレットに追加すると、ミキシング エンジンにそれらの色の正確なスペクトル バイアスが与えられるため、生成された二次および三次ミックスは現実世界の混合動作を反映します。

## ルミピグメント

The Master palette ships with the following pigments. Swatches show each pigment's typical masstone (full-strength, undiluted) appearance.

### オレンジとイエロー|スウォッチ |名前 | CI コード |家族 |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,135,20);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ピロールオレンジ | PO73 |レッド(スカーレット) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(243,114,64);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |カドミウムオレンジ | PO20 |イエロー（ボディ） |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(240,180,80);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |カドミウムイエロー | 35円 |イエロー（ボディ） |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,210,25);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |カドミウムイエローペール | PY35:ペール |イエロー(カドミウムペール) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(250,230,5);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |レモンイエロー | PY3 |イエロー（レモン） |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,155,10);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ニッケルアゾイエロー | 150円 |イエロー（ミッド） |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(180,175,45);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |グリーンゴールド | 129円 |黄緑（ゴールド） |

### アースカラー

|スウォッチ |名前 | CI コード |家族 |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(200,100,70);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |バーントシエナ | PBr7:焼けた |アース（レッドブラウン） |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(117,66,0);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |バーントアンバー | PBr7:アンバー |地球 (中立) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(205,68,35);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |生のシエナ | PBr7:生 |アース（イエローブラウン） |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(187,124,25);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |イエローオーカー | PY42 |地球（黄色） |

### グリーンズ

|スウォッチ |名前 | CI コード |家族 |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,166,81);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |フタログリーン(YS) | PG36 |グリーン (フタロイエローシェード) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(64,130,109);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ビリジアン | PG18 |グリーン (ビリジアン) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,138,112);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |テール ヴェルテ | PG23 |グリーン（アースクール） |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,110,100);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ウィンザー・グリーン (BS) | PG7 |グリーン (フタロブルーシェード) |### ブルーとシアン

|スウォッチ |名前 | CI コード |家族 |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,177,176);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |コバルトターコイズライト | PG50 |シアン (ミネラル) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,148,214);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |セルリアンブルー | PB35 |シアン (ミネラル) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,100,110);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |フタロターコイズ | PB16 |ブルー（フタロ） |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,123,194);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |コバルトブルー | PB28 |ブルー (紫がかった色) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,75,115);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ウィンザー・ブルー | PB15 |ブルー（フタロ） |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(27,63,148);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ウルトラマリン | PB29 |ブルー (紫がかった色) |

### バイオレット、マゼンタ、レッド

|スウォッチ |名前 | CI コード |家族 |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(124,65,153);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ブリリアントバイオレット | PV23 |バイオレット (ジオキサジン) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(230,90,180);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |パーマネントローズ | PV19:ローズ |マゼンタ (キナクリドン) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(190,40,120);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |キナクリドンマゼンタ | PV19:マゼンタ |マゼンタ (キナクリドン) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(160,30,65);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |永久アリザリン クリムゾン | PV19:クリムゾン |マゼンタ (キナクリドン) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,35,65);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ペリレンバイオレット | PV29 |マゼンタ (キナクリドン) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,10,45);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ペリレンマルーン | PR179 |レッド（クリムゾン） |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(215,30,60);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ピロールレッド | PR254 |レッド(スカーレット) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,55,65);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ピロールレッドライト | ピロールレッドライトPR255 |レッド(ピロールライト) |

### 黒人と白人|スウォッチ |名前 | CI コード |家族 |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(22,15,10);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |マーズブラック (ウォーム) | PBk11 |黒（火星） |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,28,12);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ペリレングリーン | PBk31 |ブラック(ペリレングリーン) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(10,18,19);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |アイボリーブラック(クール) | PBk9 |ブラック(アイボリー) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,18,18);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ランプブラック (ニュートラル) | PBk7 |黒（ランプ） |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,249,235);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |チタンホワイト（ウォーム） | PW6:暖かい |ホワイト(チタンウォーム) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,255,255);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |チタンホワイト(ニュートラル) | PW6 |ホワイト (チタンニュートラル) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,250,255);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ジンクホワイト（クール） | PW4 |ホワイト(ジンククール) |

### コントロールグレー

コントロール グレーは、ミックスの飽和度を予測どおりに下げるために使用される標準化された中和剤です。

|スウォッチ |名前 | CI コード |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,128,120);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ウォームグレー | N_WARM |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,128,128);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |ニュートラルグレー | N_ニュートラル |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,128,135);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> |クールグレー | N_COOL |

## パレットマップ

パレット マップは、アクティブなパレットを色相ホイール (36 色相セクター (10° ステップ) × 15 明度セル) として視覚化します。プライマリが追加されると、システムはセカンダリとターシャリのミックスを生成し、それらを適切なマップの位置に配置します。

セルをクリックすると、前景色として色が選択されます。 Shift キーを押しながらクリックすると、パレット ミキサーで親エンドポイントとして割り当てられます。

## パレットミキサー

パレット ミキサーは、固定の 3 段階のパイプラインを使用して 2 つの親エントリから新しい色を派生します。

1. **ブレンド**: 親 A (CCW) と親 B (CW) の間のスペクトル WGM。
2. **クロマ**: パレットのニュートラルスペクトルに向かってブレンドし、彩度を下げます。
3. **トーン**: 白を混合するか黒を混合する方向にブレンドし、明度を調整します。

トーンは最後に適用されるため、明度の調整が彩度の変化によって薄められません。値ロックおよびバンド クランプ コントロールは、結果を特定の明度レベルまたは値バンドに制限します。

混合色は **カスタム** エントリとしてパレットに保存でき、後で復元できるように完全なレシピ (親 UID、ブレンド係数、トーン、クロマ値) を保存できます。

## キャンバスのピクセルは RGBスペクトル システムは、パレットの構築と色の選択内で完全に動作します。ブラシ ストロークが適用されると、前景色 (すでにリニア RGB に変換されている) がペイントされます。キャンバスには標準の RGB ピクセル データが保存されます。

スペクトル混合により、画像データの保存方法や合成方法を変更することなく、物理的な顔料の動作と一致する方法でパレットを構築し、色を選択するエクスペリエンスが向上します。