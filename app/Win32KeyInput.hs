-- Haskell Win32 Example Programming
-- ghc -optl-mwindows Win32KeyInput.hs

module Main (
    main
) where
import Control.Exception
import Graphics.Win32
import System.Win32
import Data.Char (chr)

main = withTString "ClassName" $ \ lpszClassName -> do
    let szTitle = "Title"
    hInstance <- getModuleHandle Nothing
    myRegisterClass hInstance lpszClassName
    hWnd <- myCreateWindow hInstance lpszClassName szTitle
    myMessageLoop hWnd

myRegisterClass :: HINSTANCE -> ClassName -> IO (Maybe ATOM)
myRegisterClass hInstance lpszClassName = do
    hIcon <- loadIcon Nothing iDI_APPLICATION
    hCursor <- loadCursor Nothing iDC_ARROW
    hbrBackground <- getStockBrush wHITE_BRUSH
    registerClass (cS_HREDRAW + cS_VREDRAW, hInstance, Just hIcon, Just hCursor, Just hbrBackground, Nothing, lpszClassName)

myCreateWindow :: HINSTANCE -> ClassName -> String -> IO HWND
myCreateWindow hInstance lpszClassName szTitle = do
    hWnd <- createWindow lpszClassName szTitle wS_OVERLAPPEDWINDOW Nothing Nothing Nothing Nothing Nothing Nothing hInstance wndProc
    showWindow hWnd sW_SHOWNORMAL
    updateWindow hWnd
    return hWnd

myMessageLoop :: HWND -> IO ()
myMessageLoop hWnd = allocaMessage $ \ msg ->
    let
        whileLoop = do
            cond <- getMessage msg (Just hWnd) `catch` myGetMessageExceptionHandler
            if cond then do
                translateMessage msg
                dispatchMessage msg
                whileLoop
            else
                return ()
    in
        whileLoop

myGetMessageExceptionHandler :: IOException -> IO Bool
myGetMessageExceptionHandler _ = return False

-- getDC :: Maybe HWND -> IO HDC
-- c_GetDC :: HWND -> HDC
wndProc :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
wndProc hWnd wMsg wParam lParam
    | wMsg == wM_DESTROY = do
          sendMessage hWnd Graphics.Win32.wM_QUIT 1 0
          return 0
    | wMsg == wM_PAINT = allocaPAINTSTRUCT $ \ps -> do
          hdc <- beginPaint hWnd ps
          textOut hdc 100 100 "Hello 안녕"
          endPaint hWnd ps
          return 0
    | wMsg == wM_CHAR = do
          hdc <- c_GetDC hWnd
          textOut hdc 0 0 [chr . fromEnum $ wParam]
          return 0
    | otherwise = defWindowProc (Just hWnd) wMsg wParam lParam
