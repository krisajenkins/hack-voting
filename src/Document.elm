module Document exposing (locationHref)

import Native.Document


locationHref : () -> String
locationHref =
    Native.Document.locationHref
