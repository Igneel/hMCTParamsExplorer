
-- Тут будем хранить используемые типы.
module Types where

data MagneticField = MagneticField Double
	deriving(Show,Eq)

data HallEffect = HallEffect Double
	deriving(Show,Eq)

data Magnetoresistance = Magnetoresistance Double
	deriving(Show,Eq)

data ThinParams = ThinParams (MagneticField, Magnetoresistance, HallEffect)
	deriving (Show,Eq)