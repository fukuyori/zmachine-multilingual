;;;; Glossary for Z-machine translation
;;;; Language: :ja (Japanese / 日本語)
;;;;
;;;; Terms listed here are injected into the LLM prompt whenever they appear
;;;; in the source text, so proper nouns and game objects stay consistent
;;;; across the whole playthrough.
;;;;
;;;; Edit freely:  (add-glossary "english term" "訳語")
;;;; Then check the existing translations with (glossary-check)
;;;; and repair them with (glossary-fix).
;;;;
;;;; Note: avoid very common words (score, moves, lamp ...) - they are often
;;;; rendered with counters or particles and only create false warnings.

(in-package :zmachine)

;;; --- Locations ---------------------------------------------------
(add-glossary "West of House" "家の西側")
(add-glossary "North of House" "家の北側")
(add-glossary "South of House" "家の南側")
(add-glossary "Behind House" "家の裏側")
(add-glossary "Living Room" "居間")
(add-glossary "Kitchen" "台所")
(add-glossary "Attic" "屋根裏部屋")
(add-glossary "Cellar" "地下室")
(add-glossary "Forest Path" "森の小道")
(add-glossary "Up a Tree" "木の上")
(add-glossary "Clearing" "開けた場所")
(add-glossary "Canyon View" "峡谷の眺め")
(add-glossary "Rocky Ledge" "岩棚")
(add-glossary "Troll Room" "トロールの部屋")
(add-glossary "Maze" "迷路")
(add-glossary "Gallery" "ギャラリー")
(add-glossary "Studio" "スタジオ")
(add-glossary "Reservoir" "貯水池")
(add-glossary "Dome Room" "ドーム部屋")
(add-glossary "Temple" "神殿")
(add-glossary "Altar" "祭壇")
(add-glossary "Egyptian Room" "エジプトの部屋")
(add-glossary "Treasure Room" "宝物庫")
(add-glossary "Cyclops Room" "サイクロプスの部屋")
(add-glossary "Coal Mine" "炭鉱")
(add-glossary "white house" "白い家")

;;; --- Objects -----------------------------------------------------
(add-glossary "mailbox" "郵便受け")
(add-glossary "leaflet" "チラシ")
(add-glossary "trophy case" "トロフィーケース")
(add-glossary "brass lantern" "真鍮のランタン")
(add-glossary "elvish sword" "エルフの剣")
(add-glossary "sword" "剣")
(add-glossary "nasty knife" "危険なナイフ")
(add-glossary "rusty knife" "錆びたナイフ")
(add-glossary "rope" "ロープ")
(add-glossary "brown sack" "茶色の袋")
(add-glossary "glass bottle" "ガラスボトル")
(add-glossary "front door" "正面のドア")
(add-glossary "wooden door" "木の扉")
(add-glossary "kitchen window" "台所の窓")
(add-glossary "trap door" "仕掛け扉")     ; 既訳に「トラップドア」も混在 -> (glossary-fix) で統一
(add-glossary "jeweled egg" "宝石で飾られた卵")
(add-glossary "golden clockwork canary" "金のぜんまい仕掛けのカナリア")
(add-glossary "gold coffin" "金の棺")
(add-glossary "jeweled scepter" "宝石で飾られた笏")
(add-glossary "trunk of jewels" "宝石のトランク")
(add-glossary "crystal trident" "クリスタルの三叉戟")
(add-glossary "jade figurine" "翡翠の置物")
(add-glossary "sapphire bracelet" "サファイアのブレスレット")
(add-glossary "skeleton key" "合鍵")
(add-glossary "torch" "松明")
(add-glossary "pile of leaves" "葉の山")
(add-glossary "bloody axe" "血まみれの斧")
(add-glossary "painting" "絵画")
(add-glossary "boat" "ボート")

;;; --- Characters --------------------------------------------------
(add-glossary "grue" "グルー")
(add-glossary "thief" "泥棒")
(add-glossary "troll" "トロール")
(add-glossary "cyclops" "サイクロプス")
