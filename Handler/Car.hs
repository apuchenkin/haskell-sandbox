{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Car where

import Import
import Data.Aeson

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

instance FromJSON Car where
    parseJSON (Object v) = Car
        <$> v .:? "model"
        <*> v .:? "year"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

carAForm :: FormInput Handler Car
carAForm = Car
    <$> iopt textField "model"
    <*> iopt intField "year"

getCarsR :: Handler Value
getCarsR = do
    qs <- lookupGetParam "filter"
    let filter = join $ decodeStrict <$> (encodeUtf8 <$> qs) :: Maybe Car
    cars <- runDB $ selectList (toFilter filter) [] :: Handler [Entity Car]
    returnJson $ cars
    where
        toFilter :: Maybe Car -> [Filter Car]
        toFilter car = case car of
            Nothing            -> []
            Just Car {..}      ->
                [] ++ (
                    case carModel of
                        Just value -> [CarModel ==. Just value]
                        Nothing -> []
                ) ++ (
                    case carYear  of
                        Just value -> [CarYear  ==. Just value]
                        Nothing -> []
                )


--
--                filter notNull [
--                    CarModel ==. carModel,
--                    CarYear ==. carYear
--                ]
--
--        notNull :: Filter Car -> Bool
--        notNull Filter {} = False
--        notNull Filter {filterValue = typ} = case typ of
--            Right _ -> False
--            Left v -> case v of
--                Just v -> True
--                Nothing -> False

--
--        mapFilter :: PersistField v => (EntityField Car (Maybe v), Maybe v) -> Filter Car
--        mapFilter (field, value) = field ==. value
--
--        notNull :: PersistField v => (EntityField Car (Maybe v), Maybe v) -> Bool
--        notNull (_, value) = case value of
--            Just value -> True
--            Nothing -> False

postCarsR :: Handler Value
postCarsR = do
    car <- runInputPost carAForm
    runDB $ insert car
    returnJson car

getCarR :: CarId -> Handler Value
getCarR carId = do
    car <- runDB $ get carId
    returnJson car