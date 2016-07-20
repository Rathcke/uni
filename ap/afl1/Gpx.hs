module Gpx where

type ColourName = String
type Frame = [GpxInstr]
type Animation = [Frame]
data GpxInstr = DrawRect Integer Integer Integer Integer ColourName
              | DrawCirc Integer Integer Integer ColourName
              deriving (Eq, Show)
