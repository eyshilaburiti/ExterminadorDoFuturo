module Utils.Tabuleiro  (Tabuleiro, imprimirTabuleiros, atualizarTabuleiro, jogador1, jogador2, tabuleiro4x4, inicializarTabuleiro, movimentoValido,acharJogador) where

-- Criando o tabuleiro 
type Tabuleiro = [[String]]

tabuleiro4x4 :: Tabuleiro
tabuleiro4x4 = replicate 4 (replicate 4 "\x25A1") -- repetindo o símbolo do quadrado vazio ☐

-- Definindo os emojis dos jogadores
jogador1 :: String
jogador1 = "\x265F"  

jogador2 :: String
jogador2 = "\x2659"  

-- Inicializa o tabuleiro com os jogadores nas posições iniciais
inicializarTabuleiro :: Tabuleiro
inicializarTabuleiro = 
    [ [jogador1, "☐", "☐", "☐"]
    , ["☐", "☐", "☐", "☐"]
    , ["☐", "☐", "☐", "☐"]
    , ["☐", "☐", "☐", jogador2]
    ]

{--inicializarTabuleiro :: String -> String -> Tabuleiro
inicializarTabuleiro jogador1 jogador2 =
    [ [posicao (r, c) | c <- [0..3]] | r <- [0..3] ]
  where
    posicao (0,0) = jogador1  -- jogador 1
    posicao (3,3) = jogador2  -- jogador 2
    posicao _     = ???       -- Casas vazias
--}


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

-- Atualiza o tabuleiro com as jogadas, removendo a peça da posição antiga
atualizarTabuleiro :: Tabuleiro -> Int -> Int -> Int -> Int -> String -> Tabuleiro
atualizarTabuleiro tabuleiro linhaAntiga colunaAntiga linhaNova colunaNova jogador =
    let tabuleiroSemJogador = modificarTabuleiro tabuleiro linhaAntiga colunaAntiga "☐"  -- Remove a peça antiga
    in modificarTabuleiro tabuleiroSemJogador linhaNova colunaNova jogador  -- Adiciona a peça na nova posição

-- Atualiza o tabuleiro com as jogadas
modificarTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
modificarTabuleiro tabuleiro linha coluna jogador = 
    take linha tabuleiro ++ [take coluna (tabuleiro !! linha) ++ [jogador] ++ drop (coluna +  1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro

-- Atualiza o tabuleiro com as jogadas
{--atualizarTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
atualizarTabuleiro tabuleiro linha coluna jogador = 
    take linha tabuleiro ++ 
    [take coluna (tabuleiro !! linha) ++ [jogador] ++ drop (coluna +  1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro
--}

-- Colocar função de foco aqui )para a representação gráfica no jogo)

-- Verifica se o movimento é válido (não diagonal e apenas uma casa por vez)
movimentoValido :: (Int, Int) -> (Int, Int) -> Bool -- 2 tuplas do tipo int int (p.ini; p.final). retorna T: se pode, e F: se n pode
movimentoValido (linhaAntiga, colunaAntiga) (linhaNova, colunaNova) = -- posicao atual para oq o jogador quer
    let difLinha = abs (linhaNova - linhaAntiga)
        difColuna = abs (colunaNova - colunaAntiga)
    in (difLinha == 1 && difColuna == 0) || (difLinha == 0 && difColuna == 1)


-- Encontra a posição atual da peça do jogador no tabuleiro
acharJogador :: String -> Tabuleiro -> Maybe (Int, Int) 
acharJogador jogador tabuleiro = 
    let posicoes = [(linha, coluna) | (linha, linhaVals) <- zip [0,1,2,3] tabuleiro, -- Associa cada linha do tabuleiro ao seu índice (0, 1, 2, 3).
                                      (coluna, valor) <- zip [0,1,2,3] linhaVals, -- Associa cada coluna ao seu índice dentro da linha. 
                                      valor == jogador]-- Filtra apenas as posições onde o símbolo do jogador está.
    in if null posicoes then Nothing else Just (head posicoes)
    --null posicoes verifica se a lista de posições está vazia:
    -- se tiver vazia: retorna Nothing (o jogador não foi encontrado) se n tiver: retorna Just (head posicoes), pegando a primeira posição encontrada.