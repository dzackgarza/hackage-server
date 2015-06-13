module Distribution.Server.Features.Ranking.Render
  ( renderStarsAnon
  , renderStarsLoggedIn
  ) where

import Distribution.Package
import Text.XHtml.Strict hiding (p, name, title, content)
import Distribution.Server.Features.Users
import Distribution.Server.Users.Types (UserId(..), UserName(UserName))

renderStarsAnon :: Int -> PackageName -> (String, Html)
renderStarsAnon numStars pkgname =
  ("Stars:",
    show numStars
    +++
    form  ! [ action $    "star/" ++ unPackageName pkgname
            , method      "POST" ]
    <<
    input ! [ thetype     "submit"
            , value       "Vote for this package"
            , theclass    "text-button" ]
  )

renderStarsLoggedIn :: Int -> PackageName -> UserId -> (String, Html)
renderStarsLoggedIn numStars pkgname uid =
  ("Stars:",
    show numStars
    +++
    form  ! [ action $    "star/" ++ unPackageName pkgname
            , method      "POST" ]
    <<
    input ! [ thetype     "submit"
            , value       "Vote for this package"
            , theclass    "text-button" ]
    +++ show uid
  )
