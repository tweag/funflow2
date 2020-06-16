import Funflow (Flow, cachedIO, runFlow)

main :: IO ()
main = runFlow flow ()

flow :: Flow () ()
flow = cachedIO computation

computation :: () -> IO ()
computation = const $ putStr "\n\n====================\nIT WORKS\n====================\n\n"
