module Jogo.Tabuleiro where

-- Criando o tabuleiro 
type Tabuleiro = [[String]]

tabuleiro4x4 :: Tabuleiro
tabuleiro4x4 = replicate 4 (replicate 4 "\x1F533") -- repetindo o símbolo do quadrado vazio ☐

-- Definindo os emojis dos jogadores
jogador1 :: String
jogador1 = "\x1F98A"  

jogador2 :: String
jogador2 = "\x1F407"

semente :: String 
semente = "\x1F330"

arbusto :: String
arbusto = "\x1F331"

arvore :: String
arvore = "\x1F333"

-- Função para formatar o tabuleiro como uma String 
-- Stefane: Atualização na função, removi os delimitadores laterais 
formatarTabuleiro :: Tabuleiro -> [String]
formatarTabuleiro tabuleiro = map (\linha -> "|" ++ unwords linha ++ "|") tabuleiro
--formatarTabuleiro tabuleiro = 
    --let tamanho = length tabuleiro
    -- definindo as bordas
        --bordaSuperior = replicate (tamanho * 2 + 1) '_'  
        --bordaInferior = replicate (tamanho * 2 + 1) '¯' 
        --bordaLateral = map (\linha -> "|" ++ unwords linha ++ "|") tabuleiro
    --in bordaSuperior : bordaLateral ++ [bordaInferior]-- juntando as bordas

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
    let tabuleiroSemJogador = modificarTabuleiro tabuleiro linhaAntiga colunaAntiga "\x1F533"  -- Remove a peça antiga
    in modificarTabuleiro tabuleiroSemJogador linhaNova colunaNova jogador  -- Adiciona a peça na nova posição

-- Atualiza o tabuleiro para viagens no tempo
atualizarTabuleiroViagem :: Tabuleiro -> Tabuleiro -> Int -> Int -> String -> (Tabuleiro, Tabuleiro)
atualizarTabuleiroViagem tabuleiroDestino tabuleiroOrigem linha coluna jogador =
    let tabuleiroOrigemAtualizado = modificarTabuleiro tabuleiroOrigem linha coluna "\x1F533" -- Remove o jogador do tabuleiro de origem
        tabuleiroDestinoAtualizado = modificarTabuleiro tabuleiroDestino linha coluna jogador -- Adiciona o jogador no novo tabuleiro
    in (tabuleiroDestinoAtualizado, tabuleiroOrigemAtualizado)


-- Atualiza o tabuleiro com as jogadas
modificarTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
modificarTabuleiro tabuleiro linha coluna jogador = 
    take linha tabuleiro ++ [take coluna (tabuleiro !! linha) ++ [jogador] ++ drop (coluna +  1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro
    
-- Atualiza o tabuleiro com as jogadas
inicializarTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
inicializarTabuleiro tabuleiro linha coluna jogador = 
    take linha tabuleiro ++ [take coluna (tabuleiro !! linha) ++ [jogador] ++ drop (coluna +  1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro
    
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
plantarSementeNoTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
plantarSementeNoTabuleiro tabuleiro linha coluna planta=
    take linha tabuleiro ++ [take coluna (tabuleiro !! linha) ++ [planta] ++ drop (coluna + 1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro

contarPecas :: String -> Tabuleiro -> Int
contarPecas jogador tabuleiro = 
    length [(linha, coluna) | (linha, linhaVals) <- zip [0..] tabuleiro,
                              (coluna, valor) <- zip [0..] linhaVals,
                              valor == jogador]

verificarJogadorTabuleiro :: String -> Tabuleiro -> Bool
verificarJogadorTabuleiro jogador tabuleiro =
    any (any (== jogador)) tabuleiro