
data JPChar = Kanji {
                kanjiChar :: String
              }
            | Hiragana {
                hiragaChar :: String,
                hiragaRoman :: String
              }
            | Katakana {
                katakaChar :: String,
                katakaRoman :: String
              }

hiriganas :: [JPChar]
katakanas :: [JPChar]

