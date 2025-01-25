module Utils.Matriz  (matrizString, startTabuleiro) where
import Data.Array

-- Função para criar uma matriz de strings
matrizString :: Int -> Int -> String -> Array (Int, Int) String
matrizString x n s = array ((1, 1), (x, n)) [((i, j), s) | i <- [1..x], j <- [1..n]]

-- Função para combinar matrizes lado a lado
matrizesLadoALado :: [Array (Int, Int) String] -> String
matrizesLadoALado [] = "vazia"  -- Caso a lista esteja vazia
matrizesLadoALado (m:matrizes) =
    let
        -- Calcula os limites da primeira matriz (m)
        ((xMin, yMin), (xMax, yMax)) = bounds m
        -- Concatena as linhas de todas as matrizes lado a lado
        linhas = [unwords [unwords [matriz ! (i, j) | j <- [yMin..yMax]] ++ " |" | matriz <- (m:matrizes)] | i <- [xMin..xMax]]
    in unlines linhas

-- Função para alterar uma posição da matriz
atualizaMatriz :: Ix i => Array (i, i) e -> (i, i) -> e -> Array (i, i) e
atualizaMatriz matriz posicao novoValor = matriz // [(posicao, novoValor)]

startTabuleiro :: [Array (Int, Int) String] -> IO()
startTabuleiro matrizes = do
    -- Exibição do tabuleiro
    putStrLn (matrizesLadoALado [matrizes !! 0, matrizes !! 1, matrizes !! 2])

    -- Lendo a linha e a coluna do usuário
    putStrLn "Digite a linha:"
    linha <- readLn :: IO Int
    putStrLn "Digite a coluna:"
    coluna <- readLn :: IO Int

    updateTabuleiro matrizes linha coluna

updateTabuleiro :: [Array (Int, Int) String] -> Int -> Int -> IO()
updateTabuleiro matrizes linha coluna
    | coluna >= 0 && coluna <= 4 = do
        let novaMatriz = atualizaMatriz (matrizes !! 0) (linha, coluna) "X" -- Atualiza a matriz
        startTabuleiro [novaMatriz, matrizes !! 1, matrizes !! 2]
    | coluna <= 8 = do
        let novaMatriz = atualizaMatriz (matrizes !! 1) (linha, (-) coluna 4) "X" -- Atualiza a matriz
        startTabuleiro [matrizes !! 0, novaMatriz, matrizes !! 2]
    | coluna <= 12 = do
        let novaMatriz = atualizaMatriz (matrizes !! 2) (linha, (-) coluna 8) "X" -- Atualiza a matriz
        startTabuleiro [matrizes !! 0, matrizes !! 1, novaMatriz]
    | otherwise = putStrLn "Coluna inválida!" -- Caso contrário, exibe uma mensagem de erro