module Lib where
-- Lib contains useful library functions

-- (unsafely) change one enum into another
coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum
