module Utils.Matriz  (matrizString, startTabuleiro) where
import Data.Array

-- | Cria uma matriz de strings com dimensões especificadas.
-- 
-- === Parâmetros
-- 
-- * @x@: O número de linhas da matriz.
-- * @n@: O número de colunas da matriz.
-- * @s@: O valor (string) que será atribuído a todas as posições da matriz.
-- 
-- === Retorno
-- Retorna uma matriz (array) com dimensões @x@ por @n@, onde todas as posições contêm a string @s@.
matrizString :: Int -> Int -> String -> Array (Int, Int) String
matrizString x n s = array ((1, 1), (x, n)) [((i, j), s) | i <- [1..x], j <- [1..n]]

-- | Combina matrizes de strings lado a lado e exibe como um tabuleiro.
-- 
-- === Parâmetros
-- 
-- * @matrizes@: Uma lista de matrizes do tipo @Array (Int, Int) String@, que serão combinadas lado a lado.
-- 
-- === Retorno
-- Retorna uma string representando as matrizes combinadas, onde as matrizes são concatenadas linha por linha, separadas por barras verticais ("|"). Caso a lista de matrizes esteja vazia, retorna "vazia".
exibeTabuleiro :: [Array (Int, Int) String] -> String
exibeTabuleiro [] = "vazia"  -- Caso a lista esteja vazia
exibeTabuleiro (m:matrizes) =
    let
        -- Calcula os limites da primeira matriz (m)
        ((xMin, yMin), (xMax, yMax)) = bounds m
        -- Concatena as linhas de todas as matrizes lado a lado
        linhas = [unwords [unwords [matriz ! (i, j) | j <- [yMin..yMax]] ++ " |" | matriz <- (m:matrizes)] | i <- [xMin..xMax]]
    in unlines linhas

-- | Altera o valor de uma posição específica na matriz.
-- 
-- === Parâmetros
-- 
-- * @matriz@: A matriz original (do tipo @Array (i, i) e@) que será alterada.
-- * @posicao@: A posição da matriz a ser modificada.
-- * @novoValor@: O novo valor que será atribuído à posição especificada.
-- 
-- === Retorno
-- Retorna uma nova matriz com o valor da posição especificada alterado para @novoValor@.
atualizaMatriz :: Ix i => Array (i, i) e -> (i, i) -> e -> Array (i, i) e
atualizaMatriz matriz posicao novoValor = matriz // [(posicao, novoValor)]

-- | Inicia a exibição do tabuleiro e permite ao usuário fazer uma escolha de posição.
-- 
-- === Parâmetros
-- 
-- * @matrizes@: Uma lista de matrizes do tipo @Array (Int, Int) String@, que representam o estado do tabuleiro a ser exibido e atualizado.
-- 
-- === Retorno
-- Não retorna nenhum valor, mas exibe o tabuleiro na tela e interage com o usuário para obter a linha e a coluna para atualização do tabuleiro.
startTabuleiro :: [Array (Int, Int) String] -> IO()
startTabuleiro matrizes = do
    -- Exibição do tabuleiro
    putStrLn (exibeTabuleiro [matrizes !! 0, matrizes !! 1, matrizes !! 2])

    -- Lendo a linha e a coluna do usuário
    putStrLn "Digite a linha:"
    linha <- readLn :: IO Int
    putStrLn "Digite a coluna:"
    coluna <- readLn :: IO Int

    updateTabuleiro matrizes linha coluna

-- | Atualiza o estado do tabuleiro em uma posição específica e exibe o tabuleiro atualizado.
-- 
-- === Parâmetros
-- 
-- * @matrizes@: Uma lista de matrizes do tipo @Array (Int, Int) String@, representando o estado atual do tabuleiro.
-- * @linha@: A linha da matriz onde a atualização será feita.
-- * @coluna@: A coluna da matriz onde a atualização será feita.
-- 
-- === Retorno
-- A função não retorna um valor diretamente. Ela realiza uma chamada para a função `startTabuleiro`, que exibe o tabuleiro atualizado com a alteração realizada ou uma mensagem de erro caso a coluna seja inválida.
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