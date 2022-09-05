module App.Config where


newtype AppConfig = AppConfig {serverConfig :: ServerConfig}
  deriving (Show)

newtype ServerConfig = ServerConfig
  { port :: Integer
  }
  deriving (Show)
