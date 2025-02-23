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
    let tabuleiroOrigemAtualizado = modificarTabuleiro tabuleiroOrigem linha coluna "\x1F533" -- Chama modificarTabuleiro para substituir a posição (linha, coluna) no tabuleiro de origem por "☐" (representação de um espaço vazio). Isso simula o jogador "saindo" desse tabuleiro.
        tabuleiroDestinoAtualizado = modificarTabuleiro tabuleiroDestino linha coluna jogador -- Chama modificarTabuleiro para colocar o jogador na mesma posição (linha, coluna), mas agora no tabuleiro de destino.Isso representa o jogador "chegando" ao novo tempo.
    in (tabuleiroDestinoAtualizado, tabuleiroOrigemAtualizado) --Retorna os dois tabuleiros atualizados como uma tupla (tabuleiroDestinoAtualizado, tabuleiroOrigemAtualizado), refletindo a viagem no tempo.


-- Atualiza o tabuleiro com as jogadas
modificarTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
modificarTabuleiro tabuleiro linha coluna jogador = 
    take linha tabuleiro ++ [take coluna (tabuleiro !! linha) ++ [jogador] ++ drop (coluna +  1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro
    
-- Atualiza o tabuleiro com as jogadas
inicializarTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
inicializarTabuleiro tabuleiro linha coluna jogador = 
    take linha tabuleiro ++ [take coluna (tabuleiro !! linha) ++ [jogador] ++ drop (coluna +  1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro
    
-- Colocar função de foco aqui )para a representação gráfica no jogo)


movimentoValido :: Tabuleiro -> (Int, Int) -> (Int, Int) -> Bool
movimentoValido tabuleiro (linhaAntiga, colunaAntiga) (linhaNova, colunaNova) =
    let difLinha = abs (linhaNova - linhaAntiga)
        difColuna = abs (colunaNova - colunaAntiga)
        destinoLivre = not (posicaoOcupada tabuleiro linhaNova colunaNova) -- Verifica se o destino está vazio.
        ocupante = (tabuleiro !! linhaNova) !! colunaNova
    in (destinoLivre || ocupante /= "\x1F533") &&  -- Permite movimentação normal ou empurrão
       ((difLinha == 1 && difColuna == 0) || (difLinha == 0 && difColuna == 1))


plantarSementeNoTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
plantarSementeNoTabuleiro tabuleiro linha coluna planta =
    if not (posicaoOcupada tabuleiro linha coluna) then do 
        take linha tabuleiro ++ [take coluna (tabuleiro !! linha) ++ [planta] ++ drop (coluna + 1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro
    else tabuleiro

temPlanta :: Tabuleiro -> Int -> Int -> Bool
temPlanta tabuleiro linha coluna = 
    let valor = (tabuleiro !! linha) !! coluna
    in valor `elem` [semente, arbusto, arvore]


removerSementeNoTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
removerSementeNoTabuleiro tabuleiro linha coluna jogador =
    take linha tabuleiro ++ [take coluna (tabuleiro !! linha) ++ [jogador] ++ drop (coluna + 1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro



contarPecas :: String -> Tabuleiro -> Int
contarPecas jogador tabuleiro = 
    length [(linha, coluna) | (linha, linhaVals) <- zip [0..] tabuleiro,
                              (coluna, valor) <- zip [0..] linhaVals,
                              valor == jogador]

verificarJogadorTabuleiro :: String -> Tabuleiro -> Bool
verificarJogadorTabuleiro jogador tabuleiro =
    any (any (== jogador)) tabuleiro

-- Calcula a nova posição do jogador empurrado
novaPosicaoEmpurrado :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
novaPosicaoEmpurrado (linhaEmpurrado, colunaEmpurrado) (linhaEmpurrador, colunaEmpurrador) =
    let direcao = (linhaEmpurrado - linhaEmpurrador, colunaEmpurrado - colunaEmpurrador) -- Calcula a direção do empurrão como um vetor de deslocamento (Δlinha, Δcoluna)
        novaLinha = linhaEmpurrado + fst direcao -- fst pega o 1 elem da tupla
        novaColuna = colunaEmpurrado + snd direcao -- snd pega o 2 elem da tupla
    in if novaLinha < 0 || novaLinha >= 4 || novaColuna < 0 || novaColuna >= 4
        then Nothing  -- Se o jogador empurrado for para fora do tabuleiro, ele morre
        else Just (novaLinha, novaColuna) -- Se a posição for válida, retorna a nova posição dentro do Just.

{-empurrarJogador :: Tabuleiro -> (Int, Int) -> (Int, Int) -> String -> String -> Tabuleiro
empurrarJogador tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador jogadorEmpurrado =
    case novaPosicaoEmpurrado (linhaEmpurrado, colunaEmpurrado) (linhaEmpurrador, colunaEmpurrador) of
        Nothing -> 
            -- O empurrado morreu, então removemos ele e movemos o empurrador para sua posição
            let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado "\x1F533"  -- Remove o empurrado
                tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemEmpurrado linhaEmpurrado colunaEmpurrado jogadorEmpurrador  -- O empurrador ocupa a casa
            in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador "\x1F533"  -- Apaga a posição original do empurrador para indicar que ele se moveu.

        Just (novaLinha, novaColuna) ->
            -- O empurrado sobreviveu e se move para a nova posição
            let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado jogadorEmpurrador  -- O empurrador ocupa a posição onde estava o empurrado.
                tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemEmpurrado linhaEmpurrador colunaEmpurrador "\x1F533"  -- A posição antiga do empurrador é apagada.
                tabuleiroEmpurradoMovido = modificarTabuleiro tabuleiroComEmpurrador novaLinha novaColuna jogadorEmpurrado  -- O jogador empurrado é movido para sua nova posição.
            in tabuleiroEmpurradoMovido -}

{-empurrarJogador :: Tabuleiro -> (Int, Int) -> (Int, Int) -> String -> String -> Tabuleiro
empurrarJogador tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador jogadorEmpurrado =
    case novaPosicaoEmpurrado (linhaEmpurrado, colunaEmpurrado) (linhaEmpurrador, colunaEmpurrador) of
        Nothing -> 
            -- O empurrado sai do tabuleiro (morre), então removemos ele e movemos o empurrador para sua posição
            let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado "\x1F533"  -- Remove o empurrado
                tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemEmpurrado linhaEmpurrado colunaEmpurrado jogadorEmpurrador  -- O empurrador ocupa a casa do empurrado
            in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador "\x1F533"  -- Apaga a posição original do empurrador para indicar que ele se moveu.


        Just (novaLinha, novaColuna) -> -- TA DANDO ERRO - > loop
            let ocupanteFinal = (tabuleiro !! novaLinha) !! novaColuna  -- Quem está na posição final do empurrão?
            in if ocupanteFinal /= "\x1F533"  -- Se a posição final estiver ocupada, precisa empurrar em cadeia
                then let tabuleiroAposEmpurrao = empurrarJogador tabuleiro (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrado ocupanteFinal
                     in empurrarJogador tabuleiroAposEmpurrao (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador jogadorEmpurrado
                else 
                    -- O empurrado sobrevive e se move para a nova posição
                    let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado jogadorEmpurrador  -- O empurrador ocupa a posição onde estava o empurrado.
                        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemEmpurrado linhaEmpurrador colunaEmpurrador "\x1F533"  -- A posição antiga do empurrador é apagada.
                        tabuleiroEmpurradoMovido = modificarTabuleiro tabuleiroComEmpurrador novaLinha novaColuna jogadorEmpurrado  -- O jogador empurrado é movido para sua nova posição.
                    in tabuleiroEmpurradoMovido-}

obtemCelula :: Tabuleiro -> Int -> Int -> String
obtemCelula tabuleiro linha coluna = (tabuleiro !! linha) !! coluna

empurrarJogador :: Tabuleiro -> (Int, Int) -> (Int, Int) -> String -> String -> Tabuleiro
empurrarJogador tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador jogadorEmpurrado =
    case novaPosicaoEmpurrado (linhaEmpurrado, colunaEmpurrado) (linhaEmpurrador, colunaEmpurrador) of
        Nothing -> 
            let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado "\x1F533"
                tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemEmpurrado linhaEmpurrado colunaEmpurrado jogadorEmpurrador
            in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador "\x1F533"

        Just (novaLinha, novaColuna) ->
            -- Verifica se a nova posição está ocupada por outro jogador
            let conteudoNovo = obtemCelula tabuleiro novaLinha novaColuna
                tabuleiroPosEmpurrado = if conteudoNovo /= "\x1F533"  -- Se já houver alguém na nova posição
                    then empurrarJogador tabuleiro (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrado conteudoNovo  -- Empurra o ocupante primeiro
                    else tabuleiro
            in
                -- Move o empurrado para a nova posição e atualiza o empurrador
                let tabuleiroMovido = modificarTabuleiro tabuleiroPosEmpurrado novaLinha novaColuna jogadorEmpurrado
                    tabuleiroAtualizado = modificarTabuleiro tabuleiroMovido linhaEmpurrado colunaEmpurrado jogadorEmpurrador
                in modificarTabuleiro tabuleiroAtualizado linhaEmpurrador colunaEmpurrador "\x1F533"

empurrarador :: Tabuleiro -> (Int, Int) -> (Int, Int) -> String -> String -> Tabuleiro
empurrarador tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador jogadorEmpurrado =
    case novaPosicaoEmpurrado (linhaEmpurrado, colunaEmpurrado) (linhaEmpurrador, colunaEmpurrador) of
        --Nothing → O jogador é empurrado para fora do tabuleiro e morre e movemos, assim, o empurrador para sua posição
        Nothing -> 
            let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado "\x1F533"  -- Remove o empurrado
                tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemEmpurrado linhaEmpurrado colunaEmpurrado jogadorEmpurrador  -- O empurrador ocupa a casa
            in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador "\x1F533"  -- Limpa a posição antiga do empurrador

        Just (novaLinha, novaColuna) ->
            -- O empurrado sobreviveu e se move para a nova posição
            let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado jogadorEmpurrador  -- Empurrador ocupa o destino
                tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemEmpurrado linhaEmpurrador colunaEmpurrador "\x1F533"  -- Limpa a casa original do empurrador
                tabuleiroEmpurradoMovido = modificarTabuleiro tabuleiroComEmpurrador novaLinha novaColuna jogadorEmpurrado  -- Move o empurrado
            in tabuleiroEmpurradoMovido

-- Verifica se uma posição do tabuleiro está ocupada por algum boneco
posicaoOcupada :: Tabuleiro -> Int -> Int -> Bool
posicaoOcupada tabuleiro linha coluna =
    let valor = (tabuleiro !! linha) !! coluna 
    in valor /= "\x1F533" 

-- Verifica se há um arbustp
verificarMorteNoArbusto :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
verificarMorteNoArbusto tabuleiro linha coluna jogador =
    if (tabuleiro !! linha !! coluna) == "\x1F331" -- Verifica se a casa contém um arbusto
        then modificarTabuleiro tabuleiro linha coluna "\x1F331" -- Remove o jogador, mantendo o arbusto
        else tabuleiro

-- Verifica vitória
verificarVitoria :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> (Bool, String)
verificarVitoria tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador1 jogador2 
    | ((existeJogador tabuleiroPassado jogador1) + (existeJogador tabuleiroPresente jogador1) + (existeJogador tabuleiroFuturo jogador1)) == 1 = (True, jogador2)
    | ((existeJogador tabuleiroPassado jogador2) + (existeJogador tabuleiroPresente jogador2) + (existeJogador tabuleiroFuturo jogador2)) == 1 = (True, jogador1)
    | otherwise = (False, jogador1)

-- Verifica se existe peça de jogador em um tabuleiro específico
existeJogador :: Tabuleiro -> String -> Int
existeJogador tabuleiro jogador
    | any (elem jogador) tabuleiro = 1
    | otherwise = 0
