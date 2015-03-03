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

getCarsR :: Handler Value
getCarsR = do
    cars <- runDB $ selectList [] [] :: Handler [Entity Car]
    returnJson $ cars

postCarsR :: Handler Value
postCarsR = do
    car <- runInputPost carAForm
    runDB $ insert car
    returnJson car

getCarR :: CarId -> Handler Value
getCarR carId = do
    car <- runDB $ get carId
    returnJson $ car