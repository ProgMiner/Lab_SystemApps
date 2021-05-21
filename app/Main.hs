module Main where

import Graphics.UI.WX
import System.Posix.Internals (st_size)
import Foreign (withForeignPtr)
import Control.Monad ((>=>), mapM_)

import BTRFS (btrfsOpenFileFS, btrfsStat, btrfsReadDir)
import Util ((<.>))

main :: IO ()
main = start gui

gui :: IO ()
gui = do
    mainWindow <- frame [ text := "BTRFS viewer" ]

    -- нередактируемое поле ввода с текущим путём
    pathEntry <- entry mainWindow [text := "/", enabled := False ]

    -- список файлов
    playlist <- singleListBox mainWindow []

    -- кнопки для навигации и действий
    openButton <- button mainWindow [ text := "Open" ]
    infoButton <- button mainWindow [ text := "Info" ]
    saveButton <- button mainWindow [ text := "Save" ]
    backButton <- button mainWindow [ text := "Back" ]

    -- прикрепляем контролы к окну
    set mainWindow [
        layout := column 0 [
            margin 5 $ hfill $ widget pathEntry,
            margin 5 $ fill $ widget playlist,

            margin 5 $ row 5 [
                    hfill $ widget backButton,
                    hfill $ widget infoButton,
                    hfill $ widget saveButton,
                    hfill $ widget openButton
                ]
            ]
        ]

    return ()
