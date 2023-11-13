module Hot.Instruction.Opt.Rewrite.SomeRules (someRules) where

import Hot.Instruction.Opt.Rewrite

import qualified Data.Map as Map
import qualified Hot.Types as Hot


(-->) is os = Rule (map parse is) (map parse os)

eliminateTmpComp =
  [ [ cmd ++ " type %r a b"
    , "set type t %r"
    ] -->
    [ cmd ++ " type t a b"]
      
  | cmd <- ["add", "sub", "mul", "div", "mod", "gla", "gga"]
  ]

eliminateTmpCompBool =
  [ [ cmd ++ " type %r a b"
    , "set boolean t %r"
    ] -->
    [ cmd ++ " type t a b"]
  | cmd <- ["lt", "le", "gt", "ge", "neq", "eq"]
  ]


eliminateLocalCompBeforeRet =
  [ [ cmd ++ " * #z * *"
    , "ret t"
    ] --> 
    [ "ret t" ]
  | cmd <- ["add", "sub", "mul", "div", "mod", "lt", "le", "gt", "ge", "neq", "eq", "gla", "gga"]
  ]

eliminateLocalSetBeforeRet =
  [ [ "set * #z *"
    , "ret t"
    ] -->
    [ "ret t" ]
    
  , [ "lit * #z *"
    , "ret t"
    ] -->
    [ "ret t" ]
    
  , [ "gg * #z *"
    , "ret t"
    ] -->
    [ "ret t" ]
    
  , [ "sla * * * *"
    , "ret t"
    ] -->
    [ "ret t" ]
    
  , [ "call t fn n"
    , "set * s t"
    , "ret type"
    ] -->
    [ "call s fn n"
    , "ret type"
    ]
    
  , [ "neg * #z *"
    , "ret type"
    ] -->
    [ "ret type"]
    
  , [ "not #z *"
    , "ret type"
    ] -->
    [ "ret type"]
  ]

eliminateTmpOther =
  [ [ "neg type %r a"
    , "set type v %r"
    ] -->
    [ "neg type v a" ]
    
  , [ "set type %r a"
    , "set type b %r"
    ] -->
    [ "set type b a" ]
    
  , [ "gg type %r g"
    , "set type l %r"
    ] -->
    [ "gg type l g" ]
    
  , [ "lit type %r v"
    , "set type l %r"
    ] -->
    [ "lit type l v"]
  
  , [ "conv t1 %r t2 s"
    , "set t1 t %r"
    ] -->
    [ "conv t1 t t2 s" ]
    
  , [ "not %t a"
    , "set boolean b %t"
    ] -->
    [ "not b a"]
  
  ]

eliminateUselessJmp =
  [ [ "jmp lbl"
    , "label lbl"
    ] -->
    [ "label lbl" ]
    
  , [ "jmpt lbl r"
    , "label lbl"
    ] -->
    [ "label lbl" ]
  ]

eliminateTmpCall =
  [ [ "call %t fn n"
    , "set * a %t"
    ] -->
    [ "call a fn n" ]
  ]

compG =
  [ [ "lt type t a b"
    , "not t t"
    ] -->
    [ "ge type t a b"]
  
  , [ "le type t a b"
    , "not t t"
    ] -->
    [ "gt type t a b"]
  
  , [ "gt type t a b"
    , "not t t"
    ] -->
    [ "le type t a b"]
  
  , [ "ge type t a b"
    , "not t t"
    ] -->
    [ "lt type t a b"]
  ]

-- unfortunately wc3 handles real (==) differently than not (!=)
compNotReal = eq <> neq
  where
    types = filter (`notElem`["real", "nothing"]) $ Map.keys Hot.types
    eq =
      [ [ "eq " <> ty <> " t a b"
        , "not t t"
        ] -->
        [ "neq " <> ty <> " t a b"]
      | ty <- types
      ]
    neq =
      [ [ "neq " <> ty <> " t a b"
        , "not t t"
        ] -->
        [ "eq " <> ty <> " t a b"]
      | ty <- types
      ]
  

algebraic =
  [
    -- distributive law for integers
    -- probably never fires in real code...
    [ "mul integer %t a b"
    , "mul integer %s a c"
    , "add integer d %t %s"
    ] -->
    [ "add integer %t b c"
    , "mul integer d a %t"
    ]
    -- mul by zero for both integer and real
  , [ "lit integer %z i'0"
    , "mul integer t a %z"
    ] -->
    [ "lit integer t i'0"]
  
  , [ "lit integer %z i'0"
    , "mul integer t %z a"
    ] -->
    [ "lit integer t i'0"]
    
  , [ "lit real %z r'0"
    , "mul real t a %z"
    ] -->
    [ "lit real t r'0"]
  
  , [ "lit real %z r'0"
    , "mul real t %z a"
    ] -->
    [ "lit real t r'0"]
  
  -- mul by one for both integer and real
  , [ "lit integer %z i'1"
    , "mul integer t a %z"
    ] -->
    [ "set integer t a"]
  
  , [ "lit integer %z i'1"
    , "mul integer t %z a"
    ] -->
    [ "set integer t a"]
    
  , [ "lit real %z r'1"
    , "mul real t a %z"
    ] -->
    [ "set real t a"]
  
  , [ "lit real %z r'1"
    , "mul real t %z a"
    ] -->
    [ "set real t a"]
  
  -- add by zero for both integer and real
  , [ "lit integer %z i'0"
    , "add integer t a %z"
    ] -->
    [ "set integer t a"]
    
  , [ "lit integer %z i'0"
    , "add integer t %z a"
    ] -->
    [ "set integer t a"]
  
  , [ "lit real %z r'0"
    , "add real t a %z"
    ] -->
    [ "set real t a"]
    
  , [ "lit real %z r'0"
    , "add real t %z a"
    ] -->
    [ "set real t a"]
    
    -- (-a)+b = b-a
  , [ "neg type %t a"
    , "add type r %t b"
    ] -->
    [ "sub type r b a"]
    
    -- div by one
  , [ "lit integer %l i'1"
    , "div integer t a %l"
    ] -->
    [ "set integer t a"]
    
  , [ "lit real %l r'1"
    , "div real t a %l"
    ] -->
    [ "set real t a"]
    
    -- v*2 = 2*v = v+v
    -- for integers
  , [ "lit integer %t i'2"
    , "mul integer r s %t"
    ] -->
    [ "add integer r s s"]
    
  , [ "lit integer %t i'2"
    , "mul integer r %t s"
    ] -->
    [ "add integer r s s"]
  
    -- and for reals
  , [ "lit real %t r'2"
    , "mul real x s %t"
    ] -->
    [ "add real x s s"]
    
  , [ "lit real %t r'2"
    , "mul real x %t s"
    ] -->
    [ "add real x s s"]

  ]

removeComputeToZero =
  [ [ cmd <> " type t a b"
    , "set type 0 t"
    , "ret type"
    ] -->
    [ cmd <> " type 0 a b"
    , "ret type"
    ]
  | cmd <- ["add", "sub", "mul", "div", "mod", "gla", "gga"]
  ]

removeCompareToZero =
  [ [ cmd <> " type t a b"
    , "set boolean 0 t"
    , "ret boolean"
    ] -->
    [ cmd <> " type 0 a b"
    , "ret boolean"
    ]
  | cmd <- ["lt", "le", "gt", "ge", "neq", "eq"]
  ]


removeSetToZero =
  [ [ "set type t a"
    , "set type 0 t"
    , "ret type"
    ] -->
    [ "set type 0 a"
    , "ret type"
    ]
    
  , [ "not t a"
    , "set boolean 0 t"
    , "ret boolean"
    ] -->
    [ "not 0 a"
    , "ret boolean"
    ]
    
  , [ "neg type t a"
    , "set type 0 t"
    , "ret type"
    ] -->
    [ "neg type 0 a"
    , "ret type"
    ]
  
  , [ "gg type t a"
    , "set type 0 t"
    , "ret type"
    ] -->
    [ "gg type 0 a"
    , "ret type"
    ]
  ]

removeNoOps =
  [ [ "set type a a"
    ] --> 
    []
    
  , [ "set type a b"
    , "set type a b"
    ] -->
    [ "set type a b"]
    
  , [ "sub integer t a a"
    ] -->
    [ "lit integer t i'0"]
    
  , [ "sub real t a a"
    ] -->
    [ "lit real t r'0"]
  ]

-- everything but label and fun after ret is removed
removeUnreachableCode =
     args "ret type" 1 ["jmp", "ret"]
  <> args "ret type" 2 ["jmpt", "not"]
  <> args "ret type" 3 ["call", "lit", "neg", "set", "gg", "sg", "bind"]
  <> args "ret type" 4 ["lt", "le", "gt", "ge", "eq", "neq"]
  <> args "ret type" 4 ["div", "mul", "mod", "add", "sub"]
  <> args "ret type" 4 ["sga", "gga", "sla", "gla", "conv"]

removeEmptyJumps =
  [ [ "jmpt l1 cond"
    , "jmp l2"
    , "label l1"
    , "label l2"
    ] -->
    [ "label l1" -- just keep the labels for good measure
    , "label l2"
    ]
  , [ "lit boolean t true"
    , "jmpt lbl t"
    ] -->
    [ "lit boolean t true"
    , "jmp lbl"
    ]
  , [ "lit boolean t false"
    , "jmpt lbl t"
    ] -->
    [ "lit boolean t false"]
  ]

removeCodeAfterJump =
     args "jmp lbl" 1 ["jmp", "ret"]
  <> args "jmp lbl" 2 ["jmpt", "not"]
  <> args "jmp lbl" 3 ["call", "lit", "neg", "set", "gg", "sg", "bind"]
  <> args "jmp lbl" 4 ["lt", "le", "gt", "ge", "eq", "neq"]
  <> args "jmp lbl" 4 ["div", "mul", "mod", "add", "sub"]
  <> args "jmp lbl" 4 ["sga", "gga", "sla", "gla", "conv"]

-- used by removeCodeAfterJump, removeUnreachableCode
args b n = map (mkRule n)
  where
    mkRule n m =
        let cmd = unwords $ m:replicate n "*"
        in [b, cmd] --> [b]


someRules :: [Rule]
someRules =
       eliminateUselessJmp
    <> eliminateTmpOther
    <> eliminateLocalSetBeforeRet
    <> eliminateLocalCompBeforeRet
    <> eliminateTmpComp
    <> eliminateTmpCall
    <> eliminateTmpCompBool
    <> compNotReal
    <> compG
    <> algebraic
    <> removeComputeToZero
    <> removeSetToZero
    <> removeCompareToZero
    <> removeUnreachableCode
    <> removeEmptyJumps
    <> removeCodeAfterJump
    <> removeNoOps
