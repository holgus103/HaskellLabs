module State where 

import Control.Monad.ST.Lazy

state :: ST Int ()
state = put 10