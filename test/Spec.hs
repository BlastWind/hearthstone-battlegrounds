import Test.Tasty (defaultMain, testGroup)
import CombatTest

main :: IO ()
main = defaultMain $ testGroup "Unit Tests" [combatTestGroup]