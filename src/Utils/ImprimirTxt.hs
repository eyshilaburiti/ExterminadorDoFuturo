module Utils.ImprimirTxt where

import System.IO

imprimirTxt :: String -> IO ()
imprimirTxt caminhoArquivo = do
    handle <- openFile caminhoArquivo ReadMode  -- Abre o arquivo
    hSetEncoding handle utf8                -- Define a codificação como UTF-8
    conteudo <- hGetContents handle         -- Lê o conteúdo
    putStr conteudo                         -- Exibe o conteúdo no terminal
    hClose handle                           -- Fecha o arquivo

-- \x1F3B2  
-- \x1F331
-- \x23F3 
-- \x1F52D  