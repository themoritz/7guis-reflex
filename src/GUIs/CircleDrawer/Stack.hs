module GUIs.CircleDrawer.Stack
    ( Stack
    , StackCommand(..)
    , initialStack
    , updateStack
    , undoPossible
    , redoPossible
    , foldStack
    ) where

data Stack a = Stack [a] Int -- stack numberUndoSteps

initialStack :: Stack a
initialStack = Stack [] 0

data StackCommand a
    = StackPush a
    | StackUndo
    | StackRedo

updateStack :: StackCommand a -> Stack a -> Stack a
updateStack cmd (Stack stack undos) = case cmd of
    StackPush x -> Stack (x:drop undos stack) 0
    StackUndo   -> Stack stack (undos + 1)
    StackRedo   -> Stack stack (undos - 1)

undoPossible :: Stack a -> Bool
undoPossible (Stack stack undos) = undos < length stack

redoPossible :: Stack a -> Bool
redoPossible (Stack stack undos) = undos > 0

foldStack :: b -> (a -> b -> b) -> Stack a -> b
foldStack b0 f (Stack stack undos) =
    foldr f b0 $ drop undos stack
