{- MANUALLY FORMATTED -}
module Compiler.Reporting.Warning exposing
  ( Warning(..)
  , Context(..)
  )



-- ALL POSSIBLE WARNINGS


type Warning
  = UnusedVariable
  | MissingTypeAnnotation


type Context = Def | Pattern
