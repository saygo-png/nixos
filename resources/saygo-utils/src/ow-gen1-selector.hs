import Options.Applicative
import Shh
import Universum

data Subcommand = CmdEnable | CmdDisable

serializedIpsToBlock :: String
serializedIpsToBlock = "{ " <> intercalate "," ipsToBlock <> " }"
  where
    ipsToBlock :: [String]
    ipsToBlock = ["64.224.26.0/23", "5.42.168.0/21"]

tableName, chainName, chainName2, setName :: String
tableName = "ow-gen1-selector"
chainName = "input"
chainName2 = "output"
setName = "blocklist"

parserInfo :: ParserInfo Subcommand
parserInfo = info (helper <*> argsParser) (progDesc "Block or unblock the ams1 Overwatch server to land on gen1")
  where
    argsParser =
      subparser
        $ command "disable" (info (helper <*> pure CmdDisable) (progDesc "Enable blocking"))
        <> command "enable" (info (helper <*> pure CmdEnable) (progDesc "Enable blocking"))
        <> command "on" (info (helper <*> pure CmdEnable) (progDesc "Disable blocking"))
        <> command "off" (info (helper <*> pure CmdDisable) (progDesc "Disable blocking"))

main :: IO ()
main = do
  execParser parserInfo >>= \case
    CmdEnable -> runProc $ setup >> enableBlocklist True
    CmdDisable -> runProc $ setup >> enableBlocklist False

setup :: Proc ()
setup = addTable >> addBlocklist >> addIpsToBlocklist >> addChains
  where
    addTable, addBlocklist, addIpsToBlocklist, addChains :: Proc ()
    addTable = nft "add" "table" "inet" "ow-gen1-selector"
    addBlocklist = nft "add" "set" "inet" tableName setName "{ type ipv4_addr ; flags interval ; }"
    addIpsToBlocklist = nft "add" "element" "inet" tableName setName serializedIpsToBlock
    addChains = do
      nft "add" "chain" "inet" tableName chainName "{ type filter hook input priority 0 ; policy accept ; }"
      nft "add" "chain" "inet" tableName chainName2 "{ type filter hook output priority 0 ; policy accept ; }"

enableBlocklist :: Bool -> Proc ()
enableBlocklist True = do
  nft "add" "rule" "inet" tableName chainName "ip" "saddr" ("@" <> setName) "drop"
  nft "add" "rule" "inet" tableName chainName2 "ip" "daddr" ("@" <> setName) "drop"
enableBlocklist False = do
  nft "flush" "chain" "inet" tableName chainName
  nft "flush" "chain" "inet" tableName chainName2

nft :: Cmd
nft = exe "sudo" "nft"
