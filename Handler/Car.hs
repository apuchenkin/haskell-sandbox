{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Car where

import Import
import Data.Aeson

carAForm :: FormInput Handler Car
carAForm = Car
    <$> iopt textField "model"
    <*> iopt intField "year"

-------------------------------------------------------------------
data SomeEF e   = forall typ . PersistField typ => SomeEF { unSomeEF :: EntityField e (Maybe typ), unSomeEV :: (Maybe typ)}

getEntityFields :: Car -> [SomeEF Car]
getEntityFields Car {..} = [SomeEF CarModel carModel, SomeEF CarYear carYear]

toFilter :: Car -> [Filter Car]
toFilter car = map buildFilter $ filter notNull $ getEntityFields car where
    notNull :: SomeEF Car -> Bool
    notNull (SomeEF {unSomeEV = v}) = case v of {Nothing -> False; Just _ -> True}
    buildFilter :: SomeEF Car -> Filter Car
    buildFilter (SomeEF {unSomeEF = f, unSomeEV = v}) = f  ==. v

toAssignment :: Car -> [Update Car]
toAssignment car = map buildFilter $ filter notNull $ getEntityFields car where
    notNull :: SomeEF Car -> Bool
    notNull (SomeEF {unSomeEV = v}) = case v of {Nothing -> False; Just _ -> True}
    buildFilter :: SomeEF Car -> Update Car
    buildFilter (SomeEF {unSomeEF = f, unSomeEV = v}) = f  =. v

--------------------------------------------------------------

getCarsR :: Handler Value
getCarsR = do
    qs <- lookupGetParam "filter"
    let carFilter = join $ decodeStrict <$> (encodeUtf8 <$> qs) :: Maybe Car
    cars <- runDB $ selectList (maybeFilter carFilter) [] :: Handler [Entity Car]
    returnJson $ cars
    where
        maybeFilter :: Maybe Car -> [Filter Car]
        maybeFilter car = case car of
            Nothing  -> []
            Just c   -> toFilter c


postCarsR :: Handler Value
postCarsR = do
    carData <- runInputPost carAForm
    carId <- runDB $ insert carData
    car <- runDB $ get carId
    returnJson car

----------------------------------------------------------------

getCarR :: CarId -> Handler Value
getCarR carId = do
    car <- runDB $ get carId
    returnJson car

patchCarR :: CarId -> Handler Value
patchCarR carId = do
    carData <- runInputPost carAForm
    runDB $ update carId (toAssignment carData)
    car <- runDB $ get carId
    returnJson car

deleteCarR :: CarId -> Handler Value
deleteCarR carId = do
    runDB $ delete carId
    returnJson ()