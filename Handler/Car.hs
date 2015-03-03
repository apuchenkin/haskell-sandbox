{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Car where

import Import

instance ToJSON Car where
    toJSON Car {..} = object
        [ "model" .= carModel
        , "year"  .= carYear
        ]

instance ToJSON a => ToJSON (Entity a) where
    toJSON (Entity k a) = object
        [ "id"   .= k
        , "data" .= toJSON a
        ]

carAForm :: FormInput Handler Car
carAForm = Car
    <$> ireq textField "model"
    <*> ireq intField "year"

getCarR :: Handler Value
getCarR = do
    cars <- runDB $ selectList [] [] :: Handler [Entity Car]
    returnJson $ cars

postCarR :: Handler Value
postCarR = do
    car <- runInputPost carAForm
    runDB $ insert car
    returnJson car
