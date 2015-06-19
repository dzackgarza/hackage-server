-- | Produces HTML related to the "Stars:" section in the package page.
-- | Should only really be needed in the Ranking feature
-- | (see renderStarsHtml)
module Distribution.Server.Features.Ranking.Render
  ( renderStarsAnon
  , renderStarsAuthenticatedAs
  ) where

import Distribution.Package
import Text.XHtml.Strict hiding (p, name, title, content)
import Distribution.Server.Features.Users
import Distribution.Server.Users.Types (UserId(..), UserName(UserName))

-- When the user is not authenticated/logged in, simply
-- display the number of stars the package has.
renderStarsAnon :: Int -> PackageName -> (String, Html)
renderStarsAnon numStars pkgname =
  ( "Stars:", toHtml $ show numStars )

-- If the user is authenticated, check to see if the user has already
-- starred the package, and create a "star" or "unstar" form action
-- correspondingly.
renderStarsAuthenticatedAs :: UserId -> Int -> PackageName -> Bool -> (String, Html)
renderStarsAuthenticatedAs uid numStars pkgname alreadyStarred =
  ("Stars:",
    show numStars +++
    if alreadyStarred then
      form  ! [ action $    "unstar/" ++ unPackageName pkgname
              , method      "POST" ]
      <<
      input ! [ thetype     "submit"
              , value       "Remove star for this package"
              , theclass    "text-button" ]
    else
      form  ! [ action $    "star/" ++ unPackageName pkgname
              , method      "POST" ]
      <<
      input ! [ thetype     "submit"
              , value       "Star this package"
              , theclass    "text-button" ]
  )
