module Utils.Ranking (atualizarRanking, mostrarRanking) where

import System.IO
import System.Directory (doesFileExist, renameFile, removeFile)
import Data.List (sortOn)
import Data.Ord (Down(Down))
import Control.Exception (evaluate)

-- Define que o ranking é uma lista de tuplas (nome do jogador, pontuação)
type Ranking = [(String, Int)]

-- Caminho do arquivo onde o ranking será salvo
rankingFile :: FilePath
rankingFile = "ranking.txt"

-- Caminho do arquivo temporário para evitar corrupção ao sobrescrever o ranking
tempFile :: FilePath
tempFile = "ranking_temp.txt"

-- Lê o ranking e garante que o arquivo seja fechado corretamente
lerRanking :: IO Ranking
lerRanking = do
    -- Verifica se o arquivo de ranking existe
    existe <- doesFileExist rankingFile
    if not existe
        then return []
        else do
            conteudo <- withFile rankingFile ReadMode $ \handle -> do
                c <- hGetContents handle
                _ <- evaluate (length c)
                return c
            -- Converte cada linha do arquivo em uma tupla (jogador, pontos)
            return $ map lerLinha (lines conteudo)
  where
    -- Divide a linha em palavras, pegando o nome e convertendo os pontos para Int
    lerLinha linha = case words linha of
        (nome:pontos:_) -> (nome, read pontos)
        _               -> ("", 0)

-- Atualiza o ranking adicionando 1 ponto ao vencedor e registrando o perdedor com 0 (se ainda não existir)
atualizarRanking :: String -> String -> IO ()
atualizarRanking jogadorVencedor jogadorPerdedor = do
    ranking <- lerRanking
    let rankingComPerdedor = if jogadorPerdedor `elem` map fst ranking
                                then ranking
                                else (jogadorPerdedor, 0) : ranking
    -- Atualiza a pontuação do vencedor
    let rankingAtualizado = atualizarPontuacao jogadorVencedor 1 rankingComPerdedor
    -- Ordena o ranking em ordem decrescente de pontuação
    let rankingOrdenado = sortOn (Down . snd) rankingAtualizado

    -- Escreve os dados no arquivo temporário
    withFile tempFile WriteMode $ \handle -> do
        mapM_ (hPutStrLn handle . formatarLinha) rankingOrdenado

    -- Substitui o arquivo original pelo temporário (evita corrupção de dados)
    removeFile rankingFile
    renameFile tempFile rankingFile
  where
    formatarLinha (nome, pontos) = nome ++ " " ++ show pontos

-- Atualiza os pontos do jogador no ranking
atualizarPontuacao :: String -> Int -> Ranking -> Ranking
atualizarPontuacao jogador pontos [] = [(jogador, pontos)]
atualizarPontuacao jogador pontos ((nome, pts):resto)
    | jogador == nome = (nome, pts + pontos) : resto -- Soma pontos à pontuação atual
    | otherwise = (nome, pts) : atualizarPontuacao jogador pontos resto -- Caso contrário, continua procurando

-- Lê e imprime o ranking ordenado em ordem decrescente.
mostrarRanking :: IO ()
mostrarRanking = do
    ranking <- lerRanking
    let rankingOrdenado = sortOn (Down . snd) ranking
    let maxPontos = if null rankingOrdenado then 0 else snd (rankingOrdenado !! 0)
    putStrLn "\nRanking de Jogadores:"
    mapM_ (\(nome, pontos) -> do
        let trofeu = if pontos == maxPontos then "🏆 " else ""
        putStrLn $ trofeu ++ nome ++ ": " ++ show pontos) rankingOrdenado
