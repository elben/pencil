import Test.DocTest

main :: IO ()
main =
  doctest ["-isrc", "src/Pencil/Internal/Parser.hs"]
