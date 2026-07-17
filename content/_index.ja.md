---
title: "Linux 向けオープンソースのデジタルペイント"
toc: false
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 243e39446952d0f300b96a1af304dda1f11738a647283032b543a5cc84a8f496
---
[![gui-01](/images/screens/gui-00.jpg)](/images/screens/gui-01.jpg)

Lumi-o は、レイヤーを使ったラスター描画のために設計された、軽量で安定したソフトウェアです。Wacom タブレットのサポートにより、プロのイラスト制作に求められる滑らかで応答性の高い入力を実現します。

Lumi のパレット駆動型カラーシステムは、実在する顔料のプロファイルをスペクトル方式で混合します。可視化された色の関係をもとにパレットを構築し、明度を調整し、色を名前付きの項目として整理できます。パレットマップとパレットミキサーは、作品やプロジェクト全体で一貫した色選びに役立ちます。

GIMP のコアアーキテクチャ上に構築された Lumi-o は、アーティストによって設計・改良されています。ツールは使いやすさを保ちながら、高度なワークフローに必要なカスタマイズも提供します。高速な読み込みと保存、完全に統合された自動保存システムが、作業の流れを妨げることなく作品を守ります。

遅延読み込みにより、Lumi ははるかに少ない RAM で大規模で複雑なプロジェクトを開けます。グループが折りたたまれているとき、Lumi は保存済みの合成画像を読み込み、レイヤー、マスク、ネストされたグループはグループが展開されるまで軽量なプレースホルダーのままです。

Lumi-o は Cinnamon デスクトップを備えた Debian 13 上で開発されています。Windows から移行する場合は、まず Debian をインストールしてください。[Linux のインストール方法](/hub/install-linux/)をご覧ください。

{{< cards >}}
  {{< card link="hub/about/" title="概要" icon="information-circle" >}}
  {{< card link="hub/quick-start/" title="クイックスタート" icon="play" >}}
  {{< card link="hub/features/" title="機能" icon="book-open" >}}
{{< /cards >}}
