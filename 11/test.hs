import Test.HUnit
import Max_product

test_product4s = test [
                      "test1" ~: "product4s [1..7]" ~: product4s [1..7] ~=? [24, 120, 360, 840],
                      "test2" ~: "product4s [1..3]" ~: product4s [1..3] ~=? [],
                      "test3" ~: "product4s [1..4]" ~: product4s [1..4] ~=? [24]]

test_maxProduct4 = test [
                      "test1" ~: "maxProduct4 [1..7]" ~: maxProduct4 [1..7] ~=? 840,
                      "test2" ~: "maxProduct4 [1..3]" ~: maxProduct4 [1..3] ~=? 0,
                      "test3" ~: "maxProduct4 [1..4]" ~: maxProduct4 [1..4] ~=? 24
                      ]

test_takeUpper = test[
                 "test1" ~: "takeUpper 0 [[1,2],[3,4]]" ~: takeUpper 0 [[1,2],[3,4]] ~=? [[1,2],[4]],
                 "test2" ~: "takeUpper 0 [[1..3],[4..6]]" ~: takeUpper 0 [[1..3],[4..6]] ~=? [[1,2,3],[5,6]],
                 "test3" ~: "takeUpper 0 [[1,2],[3,4],[5,6]]" ~: takeUpper 0 [[1,2],[3,4],[5,6]] ~=? [[1,2],[4],[]]
                 ]

