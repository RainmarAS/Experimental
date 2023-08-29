module Character(Character(..)
                ,character'sColor) where
import Data.Complex
import Linear (V2(..),V4(..))
data Character = Character {x::Int, y::Int,semi_w:: Int,state :: Complex Int}
character'sColor character = V4 (fromIntegral . realPart . state $ character) 50 (fromIntegral . imagPart . state $ character) 255  