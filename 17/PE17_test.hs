import PE17
import Test.HUnit

tests = test [ "test: 12" ~: "less than 20" ~: numLetters 12 ~=? length "twelve",
               "test: 5"  ~: "less than 20" ~: numLetters 5  ~=? length "five",
               "test: 30" ~: "--ties" ~: numLetters 30 ~=? length "thirty",
               "test: 50" ~: "--ties" ~: numLetters 50 ~=? length "fifty",
               "test: 91" ~: "--ty --" ~: numLetters 91 ~=? length "ninetyone",
               "test: 37" ~: "--ty --" ~: numLetters 37 ~=? length "thirtyseven",
               "test: 100" ~: "-- hundred" ~: numLetters 100 ~=? length "onehundred",
               "test: 800" ~: "-- hundred" ~: numLetters 800 ~=? length "eighthundred",
               "test: 330" ~: "-- hundred and --ty" ~: numLetters 330 ~=? length "threehundredandthirty",
               "test: 616" ~: "-- hundred and --" ~: numLetters 616 ~=? length "sixhundredandsixteen",
               "test: 731" ~: "-- hundred and --ty --" ~: numLetters 731 ~=? length "sevenhundredandthirtyone",
               "test: 3000" ~: "-- thousand" ~: numLetters 3000 ~=? length "threethousand",
               "test: 2013" ~: "-- thousand and --" ~: numLetters 2013 ~=? length "twothousandandthirteen",
               "test: 1984" ~: "nineteen eighty four (LOL)" ~: numLetters 1984 ~=? length "onethousandninehundredandeightyfour"
             ]