module Distribution.Server.Features.Ranking.Render
  ( renderStarsAnon
  , renderStarsLoggedIn
  ) where

import Distribution.Package
import Text.XHtml.Strict hiding (p, name, title, content)
import Distribution.Server.Features.Users
import Distribution.Server.Users.Types (UserId(..), UserName(UserName))
import Distribution.Server.Features.Ranking (didUserStar)


renderStarsAnon :: Int -> PackageName -> (String, Html)
renderStarsAnon numStars pkgname =
  ( "Stars:", toHtml $ show numStars )

renderStarsLoggedIn :: Int -> PackageName -> UserId -> Bool -> (String, Html)
renderStarsLoggedIn numStars pkgname uid voted =
  ("Stars:",
    show numStars
    +++ if voted then
      form  ! [ action $    "unstar/" ++ unPackageName pkgname
              , method      "POST" ]
      <<
      input ! [ thetype     "submit"
              , value       "Remove vote for this package"
              , theclass    "text-button" ]
    else
      form  ! [ action $    "star/" ++ unPackageName pkgname
              , method      "POST" ]
      <<
      input ! [ thetype     "submit"
              , value       "Vote for this package"
              , theclass    "text-button" ]
  )
