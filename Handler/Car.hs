{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Car where

import Import
import Data.Aeson
import Control.Monad.Reader

carAForm :: FormInput Handler Car
carAForm = Car
    <$> iopt textField "model"
    <*> iopt intField "year"

-------------------------------------------------------------------
--data SomeEF e   = forall typ . PersistField typ => SomeEF { unSomeEF :: EntityField e (Maybe typ), unSomeEV :: (Maybe typ)}
data SomeEF e   = forall typ. PersistField typ => SomeEF { unSomeEF :: EntityField e typ, unSomeEV :: typ}

--getEntityFields :: Car -> [SomeEF Car]
--getEntityFields Car {..} = [SomeEF CarModel carModel, SomeEF CarYear carYear]

toFilter :: Car -> [Filter Car]
toFilter car = map buildFilter $ runReader toSomeEF car where
    buildFilter :: SomeEF Car -> Filter Car
    buildFilter (SomeEF {unSomeEF = f, unSomeEV = v}) = f  ==. v

toAssignment :: Car -> [Update Car]
toAssignment car = map buildFilter $ runReader toSomeEF car where
    buildFilter :: SomeEF Car -> Update Car
    buildFilter (SomeEF {unSomeEF = f, unSomeEV = v}) = f  =. v

toSomeEF :: Reader Car [SomeEF Car]
toSomeEF = do
    Just model <- asks carModel
    Just year  <- asks carYear
    return [SomeEF CarModel (Just model), SomeEF CarYear (Just year)]

--instance ToJSON SomeEV where
--    toJSON SomeEV {..} = toJSON unSomeEF
--
--cu :: Reader User [SomeEV]
--cu = do
--    a <- asks userIdent
--    b <- asks userPassword
--    return [SomeEV a, SomeEV b]
--
--
--postUserR :: Handler Value
--postUserR = do
--    let user = User {userIdent = "sup", userPassword = Nothing}
--    let vals = runReader cu user
--    returnJson $ vals

--------------------------------------------------------------

getCarsR :: Handler Value
getCarsR = do
    qs <- lookupGetParam "filter"
--    msort <- runInputGet $ iopt textField "sort" -- todo: add sorting
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