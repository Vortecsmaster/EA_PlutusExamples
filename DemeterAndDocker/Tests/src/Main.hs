

module Main where

import Data.Char
import Data.List
import System.Process
import Test.Tasty
import Spec
import System.Exit

-- main :: IO ()
-- main = defaultMain Spec.tests

buildParams = ["transaction",
           "build",
         "--babbage-era",
         "--testnet-magic",
         "2",
         "--tx-in",
         utxoin,
         "--tx-out",
         "addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt 50000000",
         "--tx-out-datum-hash-file",
         datumFile,
         "--change-address",
         nami,
         "--protocol-params-file",
         "protocol.params",
         "--out-file",
         "lock.unsigned"]

signParams = ["transaction",
           "sign",
         "--tx-body-file",
         "lock.unsigned",
         "--signing-key-file",
         "../../../../Wallets/Adr01.skey",
         "--testnet-magic",
         "2",
         "--out-file",
         "lock.signed"]

submitParams = ["transaction",
                "submit",
                "--testnet-magic",
                "2",
                "--tx-file",
                "lock.signed"]


txBuild :: IO ()
txBuild = do 
           (e,o,err) <- readProcessWithExitCode "cardano-cli" buildParams ""
           if e == ExitSuccess then putStrLn o else putStrLn err
           

txSign :: IO ()
txSign = do 
          (e,o,err) <- readProcessWithExitCode "cardano-cli" signParams ""
          if e == ExitSuccess then putStrLn o else putStrLn err

txSubmit :: IO ()
txSubmit = do
           (e,o,err) <- readProcessWithExitCode "cardano-cli" submitParams ""
           if e == ExitSuccess then putStrLn o else putStrLn err

main :: IO ()
main = do
        callCommand "cardano-cli query protocol-parameters --testnet-magic 2 --out-file protocol.params"
        txBuild
        txSign
        txSubmit        

        -- callCommand txbuild
        -- callCommand signTx
        -- callCommandprint e
        -- callCommand ("cardano-cli query utxo --address " ++ nami ++ " " ++ preview)

utxoin = "685429cf18b72446ecd1941ca5d0b9dd00e6b2d62c47fcdc77a3da5789944abb#0"
outputAddress = ""

outputAmmount = concat [nami," ","\"50000000\""]

datumFile = "secret.json"

preview = "--testnet-magic 2"

nami="addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt"




-- txbuild = unwords ["cardano-cli transaction build",
--                    "--babbage-era",
--                    preview,
--                    "--tx-in",
--                    utxoin,
--                    "--tx-out",
--                    outputAmmount,
--                    "--tx-out-datum-hash-file",
--                    datumFile,
--                    "--change-address",
--                    nami,
--                    "--protocol-params-file protocol.params",
--                    "--out-file lock.unsigned"]
  
-- signTx = ""
-- submitTx = ""
{-

["query", "protocol-parameters","--testnet-magic","2","--out-file","protocol.params"]

-}