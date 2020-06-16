import Funflow (Flow, cachedIO, runFlow)

main :: IO ()
main = runFlow flow ()

flow :: Flow () ()
flow = cachedIO (\() -> putStr "\n\n====================\nIT WORKS\n====================\n\n")
