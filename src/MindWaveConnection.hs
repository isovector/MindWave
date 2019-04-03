{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module MindWaveConnection (
 readMind,
 initialMindWaveInfo,
 MindWaveInfo (..),
 disconnect
) where

import qualified Control.Exception as Ex
import           Control.Monad.State
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Char (intToDigit, toUpper)
import           Data.IORef
import           Data.Word
import           System.Hardware.Serialport (SerialPort)
import qualified System.Hardware.Serialport as SP

-- | The default location the MindWave serial port is mounted (on my linux system)
mindWaveDev :: FilePath
mindWaveDev = "/dev/ttyUSB0"

-- | The serial port settings required in the MindWave spec
mindWaveSerialSettings :: SP.SerialPortSettings
mindWaveSerialSettings = SP.defaultSerialSettings { SP.commSpeed = SP.CS115200 }

-- | Sends the "magic word" for ending a connection to the MindWave.
--   Telling the MindWave to stop sending data.
sendDisconnect :: SerialPort -> IO Int
sendDisconnect sp = SP.send sp (B.pack [0xC1])

-- | Sends the "magic word" for starting a connection to the MindWave.
--   Telling the MindWave to start sending data.
sendAutoConnect :: SerialPort -> IO Int
sendAutoConnect sp = SP.send sp (B.pack [0xC2])

-- | Opens a connection to a MindWave on the default serial port location
openMindWave :: IO SerialPort
openMindWave = do
 print $ "Opening MindWave on serial port " ++ mindWaveDev
 sp <- SP.openSerial mindWaveDev mindWaveSerialSettings
 sendAutoConnect sp
 return sp

-- | Closes a connection to a MindWave on the default serial port location
closeMindWave :: SerialPort -> IO ()
closeMindWave sp = do
 print $ "Disconnecting from MindWave."
 sendDisconnect sp
 SP.closeSerial sp

-- | A type synonym for a StateT containing the serial port in use, and the
--   latest info from the MindWave, on top of the IO Monad
type MindWave a = StateT (SerialPort,MindWaveInfo) IO a

-- | A getter function for the SerialPort
getSerialPort :: MindWave SerialPort
getSerialPort = do
 (sp,_) <- get
 return sp

-- | A bracketing function to that first connects to the MindWave, performs
--   the given action, and finally disconnects from the MindWave
withMindWave :: MindWave a -> IO a
withMindWave mwa = Ex.bracket openMindWave closeMindWave $ \sp ->
 evalStateT mwa (sp,initialMindWaveInfo)

-- | A standalone IO action for disconnecting from the MindWave.
--  (This is useful if the connection is somehow left open after the program terminates)
disconnect :: IO ()
disconnect = do
 sp <- SP.openSerial mindWaveDev mindWaveSerialSettings
 print $ "Disconnecting from MindWave."
 sendDisconnect sp
 SP.closeSerial sp

-- | A pretty-print function for Word8
showWord8 :: Word8 -> String
showWord8 b = "0x" ++ [hi,lo]
  where
   hiDigit = b `div` 16
   loDigit = b - (hiDigit * 16)
   lo = toUpper (intToDigit (fromIntegral loDigit))
   hi = toUpper (intToDigit (fromIntegral hiDigit))

-- | Generates a MindWave specific Checksum from the given list of Word8
checksum :: [Word8] -> Word8
checksum bs =  complement (foldr (+) 0 bs)

-- | A packet of information from the MindWave is simple a list of Word8
data MindWavePacket = MindWavePacket [Word8]

-- | A function for reading a packet of information from the MindWave
readMindWavePacket :: MindWave MindWavePacket
readMindWavePacket = do
 sync <- readByte
 case sync of
   0xAA -> do
     sync' <- readByte
     case sync' of
       0xAA -> readPayload
       _ -> readMindWavePacket
   _ -> readMindWavePacket

-- | A helper function for reading the payload from the MindWave
readPayload :: MindWave MindWavePacket
readPayload = do
  datalength <- readByte
  case (datalength > 169) of
    True -> readMindWavePacket
    False -> do
      pl <- readBytes datalength
      cs <- readByte
      case (checksum pl == cs) of
        False -> readMindWavePacket
        True -> return (MindWavePacket pl)

-- | A helper function for reading a single Word8 from the MindWave
readByte :: MindWave Word8
readByte = do
 sp <- getSerialPort
 bs <- lift $ SP.recv sp 1
 case (B.unpack bs) of
  [] -> readByte
  [x] -> return x
  _ -> error "Multiple bytes returned"

-- | A helper function for reading a specifc number of Word8 from the MindWave
readBytes :: Word8 -> MindWave [Word8]
readBytes x = do
  bs <- doReadBytes x []
  return $ reverse bs

-- | A recursive hepler function for reading a specifc number of Word8 from the MindWave
doReadBytes :: Word8 -> [Word8] -> MindWave [Word8]
doReadBytes 0 bs = return bs
doReadBytes n bs = do
 byte <- readByte
 doReadBytes (n-1) (byte:bs)

-- | A row of date from the MindWave
data MindWaveDataRow = MindWaveDataRow Int Word8 [Word8]

instance Show MindWaveDataRow where
 show (MindWaveDataRow ec c v) = show ec ++ ":" ++ showWord8 c ++ concat (map (\b -> ' ':showWord8 b) v)

-- | Parse a packet of information from the MindWave into a list of data rows.
parseMindWavePacket :: MindWavePacket -> [MindWaveDataRow]
parseMindWavePacket (MindWavePacket []) = []
parseMindWavePacket (MindWavePacket pl) = (MindWaveDataRow excode code value):parseMindWavePacket (MindWavePacket rest)
 where
  (excode,pl') = countExcodes pl 0
  (code,pl'') = (head pl', tail pl')
  (value, rest) = if code > 0x7F
                   then (take (fromIntegral $ head pl'') (tail pl''), drop (fromIntegral $ head pl'') (tail pl''))
                   else ([head pl''], tail pl'')

-- | A helper function for counting the number of "Excodes" at the begining of a list of Word8
countExcodes :: [Word8] -> Int -> (Int, [Word8])
countExcodes (0x55:bs) n = countExcodes bs (n+1)
countExcodes bs n = (n, bs)

{-
prettyPrintData :: MindWave ()
prettyPrintData = sequence_ $ repeat $ do
  mwp <- readMindWavePacket
  lift $ print (map prettyPrint (parseMindWavePacket mwp))

printData :: MindWave ()
printData = (lift $ clearScreen >> hideCursor) >> (sequence_ $ repeat $ do
  mwp <- readMindWavePacket
  (sp,mwi) <- get
  let mwi' = foldr updateState mwi (parseMindWavePacket mwp)
  lift $ setCursorPosition 0 0 >> print mwi'
  put (sp,mwi'))

main :: IO ()
main = withMindWave printData

prettyPrint :: MindWaveDataRow -> String
prettyPrint mwp@(MindWaveDataRow excode code value) =
 case excode of
  0 -> case code of
        0x02 -> "POOR_SIGNAL Quality: " ++ show (head value)
        0x04 -> "ATTENTION eSense: " ++ show (head value)
        0x05 -> "MEDITATION eSense: " ++ show (head value)
        0x16 -> "Blink Strength: " ++ show (head value)
        0x55 -> "EXCODE " ++ show value
  0x80 -> "RAW: " ++ hexValue 2 value
  0x83 -> "ASIC_EEG_POWER: " ++ eegValue value
  0xAA -> "SYNC " ++ show value
  0xD0 -> "Connected to Headset " ++ hexValue 2 value
        0xD1 -> case (length value) of
                 0 -> "No Headsets found"
                 2 -> "Headset " ++ hexValue 2 value ++ " not found"
                 _ -> show mwp
        0xD2 -> "Disconnected from Headset " ++ hexValue 2 value
        0xD3 -> "Request Denied"
        0xD4 -> case (head value) of
                 0 -> "Dongle in Standy Mode"
                 _ -> "Scanning for Headset"
  _ -> show mwp
  _ -> show mwp
-}

-- | A helper function for pretty printing Word8 in Hex format
hexValue :: Int -> [Word8] -> String
hexValue n ws = "0x" ++ concat (map (\w -> drop 2 (showWord8 w)) (take n ws))


buildWord32 :: Word8 -> Word8 -> Word8 -> Word32
buildWord32 b c d =
  (flip shift 16 $ fromIntegral b) .|.
  (flip shift 8  $ fromIntegral c) .|.
   fromIntegral d

{-
eegValue :: [Word8] -> String
eegValue [d1,d2,d3,t1,t2,t3,la1,la2,la3,ha1,ha2,ha3,lb1,lb2,lb3,hb1,hb2,hb3,lg1,lg2,lg3,mg1,mg2,mg3] = d ++ t ++ la ++ ha ++ lb ++ hb ++ lg ++ mg
 where
  d = "delta:" ++ hexValue 3 [d1,d2,d3] ++ " "
  t = "theta:" ++ hexValue 3 [t1,t2,t3] ++ " "
  la = "low-alpha:" ++ hexValue 3 [la1,la2,la3] ++ " "
  ha = "high-alpha:" ++ hexValue 3 [ha1,ha2,ha3] ++ " "
  lb = "low-beta:" ++ hexValue 3 [lb1,lb2,lb3] ++ " "
  hb = "high-beta:" ++ hexValue 3 [hb1,hb2,hb3] ++ " "
  lg = "low-gamma:" ++ hexValue 3 [lg1,lg2,lg3] ++ " "
  mg = "mid-gamma:" ++ hexValue 3 [mg1,mg2,mg3] ++ " "
eegValue ws = "error " ++ show ws
-}

-- | MindWaveInfo consits of the data that is sent from the MindWave
data MindWaveInfo = MindWaveInfo
  { dongle_status  :: String
  , last_message   :: String
  , poor_signal    :: Word8
  , attention      :: Word8
  , meditation     :: Word8
  , blink_strength :: Word8
  , raw_value      :: String
  , delta          :: Float
  , theta          :: Float
  , low_alpha      :: Float
  , high_alpha     :: Float
  , low_beta       :: Float
  , high_beta      :: Float
  , low_gamma      :: Float
  , mid_gamma      :: Float
  , unknown_code   :: String
  } deriving Show

-- | Before connecting to the MindWave, we have no information.
initialMindWaveInfo :: MindWaveInfo
initialMindWaveInfo = MindWaveInfo "" "" 0 0 0 0 "" 0 0 0 0 0 0 0 0 ""

-- | For any data row we recieve, we can update the info accordingly
updateState :: MindWaveDataRow -> MindWaveInfo -> MindWaveInfo
updateState mwp@(MindWaveDataRow excode code value) mwi =
  case excode of
   0 ->
     case code of
       0x02 -> mwi { poor_signal = head value }
       0x04 -> mwi { attention = head value }
       0x05 -> mwi { meditation = head value }
       0x16 -> mwi { blink_strength = head value }
       0x80 -> mwi { raw_value = hexValue 2 value }
       0x83 -> updateEegValue value mwi
       0xD0 -> mwi { dongle_status = "Connected to Headset " ++ hexValue 2 value }
       0xD1 ->
         case (length value) of
           0 -> mwi { last_message = "No Headsets found                 " }
           2 -> mwi { last_message = "Headset " ++ hexValue 2 value ++ " not found" }
           _ -> mwi { unknown_code = show mwp }
       0xD2 -> mwi { last_message = "Disconnected from Headset " ++ hexValue 2 value }
       0xD3 -> mwi { last_message = "Request Denied" }
       0xD4 ->
         case (head value) of
           0 -> mwi { dongle_status = "Dongle in Standy Mode" }
           _ -> mwi { dongle_status = "Scanning for Headset " }
       _ -> mwi { unknown_code = show mwp }
   _ -> mwi { unknown_code = show mwp }

-- | A helper function for extracting EEG specific values from the MindWave
updateEegValue :: [Word8] -> MindWaveInfo -> MindWaveInfo
updateEegValue
  [ d1,  d2,  d3
  , t1,  t2,  t3
  , la1, la2, la3
  , ha1, ha2, ha3
  , lb1, lb2, lb3
  , hb1, hb2, hb3
  , lg1, lg2, lg3
  , mg1, mg2, mg3
  ] mwi
  = mwi
      { delta = d
      , theta = t
      , low_alpha = la
      , high_alpha = ha
      , low_beta = lb
      , high_beta = hb
      , low_gamma = lg
      , mid_gamma = mg
      }
 where
  d  = log $ fromIntegral $ buildWord32 d1 d2 d3
  t  = log $ fromIntegral $ buildWord32 t1 t2 t3
  la = log $ fromIntegral $ buildWord32 la1 la2 la3
  ha = log $ fromIntegral $ buildWord32 ha1 ha2 ha3
  lb = log $ fromIntegral $ buildWord32 lb1 lb2 lb3
  hb = log $ fromIntegral $ buildWord32 hb1 hb2 hb3
  lg = log $ fromIntegral $ buildWord32 lg1 lg2 lg3
  mg = log $ fromIntegral $ buildWord32 mg1 mg2 mg3
updateEegValue ws mwi = mwi { unknown_code = show ws }

-- | Store MindWaveInfo in an IORef, and continually update it from the MindWave
readFromMindWave :: IORef MindWaveInfo -> MindWave ()
readFromMindWave ref = sequence_ $ repeat $ do
  mwp <- readMindWavePacket
  (sp,mwi) <- get
  let mwi' = foldr updateState mwi (parseMindWavePacket mwp)
  lift $ writeIORef ref mwi'
  put (sp,mwi')

-- | An overall function for connecting the MindWave and continually updating the
--   info stored in an IORef
readMind :: IORef MindWaveInfo -> IO ()
readMind ref = withMindWave $ readFromMindWave ref
