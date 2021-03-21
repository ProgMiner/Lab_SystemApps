module Main where

import Graphics.UI.WX

{-

wxHaskell demo (eaxPlayer) v 0.1
(c) Alexandr A Alexeev 2011 | http://eax.me/

-}

main :: IO ()
main = start gui

gui :: IO ()
gui = do
  -- форма, на которой будут лежать все наши контролы
  wnd <- frame [ text := "wxHaskell demo (c) afiskon 2011 http://eax.me/" ]
 
  -- нередактируемое поле ввода с названием текущей песни
  txtTitle <- entry wnd [text := "Welcome to eaxPlayer!" , enabled := False ]
  -- ползунок громкости, выставленный на значении 50/100
  volumeSlider <- hslider wnd False {- hide labels -} 1 100 [ selection:= 50 ]

  -- несколько кнопок фиксированного размера
  beginButton <- button wnd [ text := "|<", clientSize := sz 25 25 ]
  playButton <- button wnd [ text := ">", clientSize := sz 25 25 ]
  endButton <- button wnd [ text := ">|", clientSize := sz 25 25 ]
 
  -- ползунок, отобращающий текущую позицию в играющем треке
  positionSlider <- hslider wnd False 1 100 [ ]
 
  -- чекбоксы loop, shuffle и mute
  loopCheckbox <- checkBox wnd [ text := "L" ]
  shuffleCheckbox <- checkBox wnd [ text := "S" ]
  muteCheckbox <- checkBox wnd [ text := "M" ]

  -- список песен
  playlist <- singleListBox wnd []

  -- кнопки для управления плейлистом
  addButton <- button wnd [ text := "Add" ]
  delButton <- button wnd [ text := "Delete" ]
  saveButton <- button wnd [ text := "Save" ]
  openButton <- button wnd [ text := "Open" ]
  openUrlButton <- button wnd [ text := "Open URL" ]

  -- прикрепляем контролы к окну
  set wnd [
    layout :=
      column 0 [
        {- верхний ряд -}
        margin 5 $ row 5 [
          -- название песни
          hfill $ minsize (sz 150 0) $ widget txtTitle,
          -- регулятор громкости
          widget volumeSlider
        ],
       
        {- средний ряд -}
        margin 5 $ row 5 [
          -- слева: кнопки
          row 5 [
            widget beginButton,        -- предыдущая песня
            widget playButton,         -- играть / пауза
            widget endButton           -- следующая песня
          ],
          -- по центру: позиция в текущей песне
          hfill $ widget positionSlider,
          -- справа: чекбоксы
          row 5 [
            widget loopCheckbox,       -- крутить одну песню
            widget shuffleCheckbox,    -- перемешать песни
            widget muteCheckbox        -- выключить звук
          ]
        ],
       
        {- плейлист -}
        margin 5 $ fill $ widget playlist,
       
        {- нижний ряд -}
        margin 5 $ row 5 [
          hfill $ widget addButton,    -- добавить трек
          hfill $ widget delButton,    -- удалить трек
          hfill $ widget saveButton,   -- сохранить плейлист
          hfill $ widget openButton,   -- открыть плейлист
          hfill $ widget openUrlButton -- открыть url
        ]
      ]
    ]
  return ()
