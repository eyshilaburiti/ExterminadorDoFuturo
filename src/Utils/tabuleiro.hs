module Utils.Tabuleiro  (Tabuleiro, imprimirTabuleiros, atualizarTabuleiro, jogador1, jogador2, tabuleiro4x4) where

-- Criando o tabuleiro 
type Tabuleiro = [[String]]

tabuleiro4x4 :: Tabuleiro
tabuleiro4x4 = replicate 4 (replicate 4 "\x25A1") -- repetindo o símbolo do quadrado usado

-- Definindo os emojis dos jogadores
jogador1 :: String
jogador1 = "\x265F"

jogador2 :: String
jogador2 = "\x2659"

-- Função para formatar o tabuleiro como uma String 
formatarTabuleiro :: Tabuleiro -> [String]
formatarTabuleiro tabuleiro = 
    let tamanho = length tabuleiro
    -- definindo as bordas
        bordaSuperior = replicate (tamanho * 2 + 1) '_'  
        bordaInferior = replicate (tamanho * 2 + 1) '¯' 
        bordaLateral = map (\linha -> "|" ++ unwords linha ++ "|") tabuleiro
    in bordaSuperior : bordaLateral ++ [bordaInferior]-- juntando as bordas

-- Função para imprimir os 3 tabuleiros
imprimirTabuleiros :: Tabuleiro -> Tabuleiro -> Tabuleiro -> IO ()
imprimirTabuleiros t1 t2 t3 = do
    -- formatando os 3 tabuleiros para a impressão
    let tabuleiro1 = formatarTabuleiro t1
        tabuleiro2 = formatarTabuleiro t2
        tabuleiro3 = formatarTabuleiro t3

    --organizando os tabuleiros para a impressao
    mapM_ (\(linha1, linha2, linha3) -> putStrLn $ linha1 ++ "   " ++ linha2 ++ "   " ++ linha3)
          (zip3 tabuleiro1 tabuleiro2 tabuleiro3)

-- Atualiza o tabuleiro com as jogadas
atualizarTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
atualizarTabuleiro tabuleiro linha coluna jogador = 
    take linha tabuleiro ++ 
    [take coluna (tabuleiro !! linha) ++ [jogador] ++ drop (coluna +  1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro

-- Colocar função de foco aqui )para a representação gráfica no jogo)