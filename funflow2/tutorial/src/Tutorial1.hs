{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -F -pgmF inlitpp #-}

```haskell top hide
-- Instances of AskInliterate to display properly things
import Lib ()
```

# funflow2: User Guide

We need to use the following pragmas:

```haskell
{-# LANGUAGE TypeApplications #-}
```

First we import funflow2:

```haskell top
import Funflow (Flow, pureFlow, runFlow)
```

We then define a flow:

```haskell top
flow :: Flow () String
flow = pureFlow $ \() -> "Hello world"
```

And finally we can run in a IO monad:

```haskell eval twocol
runFlow @() @String flow ()
```

## Complete example

```haskell
{-# LANGUAGE TypeApplications #-}

import Funflow (Flow, pureFlow, runFlow)

flow :: Flow () String
flow = pureFlow $ \() -> "Hello world"

main = runFlow @() @String flow ()
```