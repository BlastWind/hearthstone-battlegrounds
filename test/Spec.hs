import ViewTest (viewTestGroup)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Unit Tests" [viewTestGroup]