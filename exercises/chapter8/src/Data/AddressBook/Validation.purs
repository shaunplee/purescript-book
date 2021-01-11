module Data.AddressBook.Validation where

import Prelude
import Data.AddressBook (Address, Person, PhoneNumber, PhoneType(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, invalid, toEither)

data Field
  = FirstNameField
  | LastNameField
  | StreetField
  | CityField
  | StateField
  | PhoneField PhoneType

derive instance genericField :: Generic Field _

instance showField :: Show Field where
  show = genericShow

instance eqField :: Eq Field where
  eq = genericEq

data ValidationError
  = ValidationError String Field

type Errors
  = Array ValidationError

nonEmpty :: Field -> String -> V Errors String
nonEmpty field "" = invalid [ ValidationError "Field cannot be empty" field ]

nonEmpty _ value = pure value

validatePhoneNumbers :: String -> Array PhoneNumber -> V Errors (Array PhoneNumber)
validatePhoneNumbers field [] = invalid [ ValidationError "Field must contain at least one value" (PhoneField OtherPhone) ]

validatePhoneNumbers _ phones = traverse validatePhoneNumber phones

lengthIs :: Field -> Int -> String -> V Errors String
lengthIs field len value
  | length value /= len = invalid [ ValidationError ("Field must have length " <> show len) field ]

lengthIs _ _ value = pure value

phoneNumberRegex :: Either String Regex
phoneNumberRegex = regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags

matches :: Field -> Either String Regex -> String -> V Errors String
matches _ (Right regex) value
  | test regex value = pure value

matches field (Left error) _ = invalid [ ValidationError error field ]

matches field _ _ = invalid [ ValidationError "Field did not match the required format" field ]

validateAddress :: Address -> V Errors Address
validateAddress a =
  address <$> nonEmpty StreetField a.street
    <*> nonEmpty CityField a.city
    <*> lengthIs StateField 2 a.state

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber pn =
  phoneNumber <$> pure pn."type"
    <*> matches (PhoneField pn."type") phoneNumberRegex pn.number

validatePerson :: Person -> V Errors Person
validatePerson p =
  person <$> nonEmpty FirstNameField p.firstName
    <*> nonEmpty LastNameField p.lastName
    <*> validateAddress p.homeAddress
    <*> validatePhoneNumbers "Phone Numbers" p.phones

validatePerson' :: Person -> Either Errors Person
validatePerson' p = toEither $ validatePerson p
