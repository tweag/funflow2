import Funflow.Base (Flow, runFlow)
import Funflow.Flows (cachedIO)

main :: IO ()
main = runFlow flow ()

-- main :: IO ()
-- main = print $ runFlow flow input
-- main = print $ runFlowWithProps props flow input

flow :: Flow () ()
flow = cachedIO (\() -> putStr "\n\n====================\nIT WORKS\n====================\n\n")
