module Mit.Git.GitConflict
  ( GitConflict (..),
    parseGitConflict,
    GitConflictXY (..),
    parseGitConflictXY,
  )
where

import Data.Text qualified as Text
import Mit.Prelude

data GitConflict
  = GitConflict GitConflictXY Text
  deriving stock (Eq, Show)

-- FIXME some other color, magenta?

-- FIXME
--
-- error: The following untracked working tree files would be overwritten by merge:
--         administration-client/administration-client.cabal
--         aeson-simspace/aeson-simspace.cabal
--         attack-designer/api/attack-designer-api.cabal
--         attack-designer/db/attack-designer-db.cabal
--         attack-designer/server/attack-designer-server.cabal
--         attack-integrations/attack-integrations.cabal
--         authz/simspace-authz.cabal
--         caching/caching.cabal
--         common-testlib/common-testlib.cabal
--         db-infra/db-infra.cabal
--         db-infra/migrations/0_migrate-rich-text-images-to-minio/migrate-rich-text-images-to-minio.cabal
--         db-infra/migrations/2.0.0.1010_migrate-questions-into-content-modules/range-data-server-migrate-questions-into-content-modules.cabal
--         db-infra/migrations/2.0.0.19_migrate-hello-table/range-data-server-migrate-hello-table.cabal
--         db-infra/migrations/2.0.0.21_migrate-puppet-yaml-to-text/range-data-server-migrate-puppet-yaml-to-text.cabal
--         db-infra/migrations/2.0.0.9015_migrate-refresh-stocks/range-data-server-migrate-refresh-stocks.cabal
--         db-infra/migrations/shared/range-data-server-migration.cabal
-- Please move or remove them before you merge.
-- Aborting
parseGitConflict :: Text -> Maybe GitConflict
parseGitConflict line = do
  [xy, name] <- Just (Text.words line)
  GitConflict <$> parseGitConflictXY xy <*> Just name

data GitConflictXY
  = AA -- both added
  | AU -- added by us
  | DD -- both deleted
  | DU -- deleted by us
  | UA -- added by them
  | UD -- deleted by them
  | UU -- both modified
  deriving stock (Eq, Show)

parseGitConflictXY :: Text -> Maybe GitConflictXY
parseGitConflictXY = \case
  "AA" -> Just AA
  "AU" -> Just AU
  "DD" -> Just DD
  "DU" -> Just DU
  "UA" -> Just UA
  "UD" -> Just UD
  "UU" -> Just UU
  _ -> Nothing
