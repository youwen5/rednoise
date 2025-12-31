module BlazeSupport where

import Text.Blaze.Internal (Attribute, AttributeValue, attribute)

-- blaze-html's attributes are not exhaustive, so we need to define a few of our own

as, crossorigin, lazy :: AttributeValue -> Attribute
as = attribute "as" " as=\""
crossorigin = attribute "crossorigin" " crossorigin=\""
lazy = attribute "lazy" " lazy=\""
