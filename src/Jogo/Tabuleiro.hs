module Jogo.Tabuleiro where

-- Criando o tabuleiro 
type Tabuleiro = [[String]]

tabuleiro4x4 :: Tabuleiro
tabuleiro4x4 = replicate 4 (replicate 4 espacoVazio) -- repetindo o símbolo do quadrado vazio ☐

-- Definindo os emojis dos jogadores
espacoVazio :: String
espacoVazio = "\x1F533"

jogador1 :: String
jogador1 = "\x26AA"   

jogador2 :: String
jogador2 = "\x26AB"

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
    let tabuleiroSemJogador = modificarTabuleiro tabuleiro linhaAntiga colunaAntiga espacoVazio  -- Remove a peça antiga
    in modificarTabuleiro tabuleiroSemJogador linhaNova colunaNova jogador  -- Adiciona a peça na nova posição

-- Atualiza o tabuleiro para viagens no tempo
atualizarTabuleiroViagem :: Tabuleiro -> Tabuleiro -> Int -> Int -> String -> (Tabuleiro, Tabuleiro)
atualizarTabuleiroViagem tabuleiroDestino tabuleiroOrigem linha coluna jogador =
    let tabuleiroOrigemAtualizado = modificarTabuleiro tabuleiroOrigem linha coluna espacoVazio -- Chama modificarTabuleiro para substituir a posição (linha, coluna) no tabuleiro de origem por "☐" (representação de um espaço vazio). Isso simula o jogador "saindo" desse tabuleiro.
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

movimentoValido :: Tabuleiro -> (Int, Int) -> (Int, Int) -> Bool
movimentoValido tabuleiro (linhaAntiga, colunaAntiga) (linhaNova, colunaNova) =
    let difLinha = abs (linhaNova - linhaAntiga)
        difColuna = abs (colunaNova - colunaAntiga)
        destinoLivre = not (posicaoOcupada tabuleiro linhaNova colunaNova) -- Verifica se o destino está vazio.
        ocupante = (tabuleiro !! linhaNova) !! colunaNova
    in (destinoLivre || ocupante /= espacoVazio) &&  -- Permite movimentação normal ou empurrão
       ((difLinha == 1 && difColuna == 0) || (difLinha == 0 && difColuna == 1))


plantarSementeNoTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
plantarSementeNoTabuleiro tabuleiro linha coluna planta =
    if not (posicaoOcupada tabuleiro linha coluna) then do 
        take linha tabuleiro ++ [take coluna (tabuleiro !! linha) ++ [planta] ++ drop (coluna + 1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro
    else tabuleiro

removerSementeNoTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
removerSementeNoTabuleiro tabuleiro linha coluna planta =
    if plantaCerta tabuleiro  linha coluna planta then
        take linha tabuleiro ++ [take coluna (tabuleiro !! linha) ++ [espacoVazio] ++ drop (coluna + 1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro
    else tabuleiro

plantaCerta :: Tabuleiro -> Int -> Int -> String -> Bool
plantaCerta tabuleiro linha coluna planta =
    let valor = (tabuleiro !! linha) !! coluna
    in valor == planta

temPlanta :: Tabuleiro -> Int -> Int -> Bool
temPlanta tabuleiro linha coluna = 
    let valor = (tabuleiro !! linha) !! coluna
    in valor `elem` [semente, arbusto, arvore]


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

obtemCelula :: Tabuleiro -> Int -> Int -> String
obtemCelula tabuleiro linha coluna = (tabuleiro !! linha) !! coluna

empurrarJogador :: Tabuleiro -> (Int, Int) -> (Int, Int) -> String -> String -> Tabuleiro
empurrarJogador tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador jogadorEmpurrado =
    case novaPosicaoEmpurrado (linhaEmpurrado, colunaEmpurrado) (linhaEmpurrador, colunaEmpurrador) of
        Nothing -> morteBordas tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador
        Just (novaLinha, novaColuna) -> empurrao tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador jogadorEmpurrado

-- Função para tratar a morte do jogador ao ser empurrado para fora do tabuleiro
morteBordas :: Tabuleiro -> (Int, Int) -> (Int, Int) -> String -> Tabuleiro
morteBordas tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador =
    let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado espacoVazio
        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemEmpurrado linhaEmpurrado colunaEmpurrado jogadorEmpurrador
    in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio

-- Função para processar o empurrão, considerando vários casos
empurrao :: Tabuleiro -> (Int, Int) -> (Int, Int) -> (Int, Int) -> String -> String -> Tabuleiro
empurrao tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador jogadorEmpurrado =
    let conteudoNovo = obtemCelula tabuleiro novaLinha novaColuna
    in case conteudoNovo of
        _ | conteudoNovo == jogadorEmpurrado -> paradoxo tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador
          | conteudoNovo == arbusto -> mortePorArbusto tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador
          | conteudoNovo == arvore && jogadorEmpurrado /= arvore -> empurraArvore tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador jogadorEmpurrado
          | jogadorEmpurrado == arvore -> arvoreCai tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador
          | otherwise -> empurraoNormal tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador jogadorEmpurrado

paradoxo :: Tabuleiro -> (Int, Int) -> (Int, Int) -> (Int, Int) -> String -> Tabuleiro
paradoxo tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador =
    let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado espacoVazio
        tabuleiroSemAmbos = modificarTabuleiro tabuleiroSemEmpurrado novaLinha novaColuna espacoVazio
        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemAmbos linhaEmpurrado colunaEmpurrado jogadorEmpurrador
    in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio

-- Caso de morte por arbusto
mortePorArbusto :: Tabuleiro -> (Int, Int) -> (Int, Int) -> String -> Tabuleiro
mortePorArbusto tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador =
    let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado espacoVazio
        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemEmpurrado linhaEmpurrado colunaEmpurrado jogadorEmpurrador
    in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio

-- Caso onde um jogador empurra uma árvore
empurraArvore :: Tabuleiro -> (Int, Int) -> (Int, Int) -> (Int, Int) -> String -> String -> Tabuleiro
empurraArvore tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador jogadorEmpurrado =
    let tabuleiroSemArvore = modificarTabuleiro tabuleiro novaLinha novaColuna espacoVazio
        tabuleiroComEmpurrado = modificarTabuleiro tabuleiroSemArvore novaLinha novaColuna jogadorEmpurrado
        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroComEmpurrado linhaEmpurrado colunaEmpurrado jogadorEmpurrador
    in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio

-- Caso onde uma árvore cai e pode matar um jogador
arvoreCai :: Tabuleiro -> (Int, Int) -> (Int, Int) -> (Int, Int) -> String -> Tabuleiro
arvoreCai tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador =
    let tabuleiroSemArvore = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado espacoVazio
        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemArvore linhaEmpurrado colunaEmpurrado jogadorEmpurrador
        tabuleiroMovido = modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio
        conteudoNovo = obtemCelula tabuleiro novaLinha novaColuna
    in if conteudoNovo /= espacoVazio && conteudoNovo /= arbusto && conteudoNovo /= semente
        then modificarTabuleiro tabuleiroMovido novaLinha novaColuna espacoVazio
        else tabuleiroMovido

-- Caso normal de empurrão
empurraoNormal :: Tabuleiro -> (Int, Int) -> (Int, Int) -> (Int, Int) -> String -> String -> Tabuleiro
empurraoNormal tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador jogadorEmpurrado =
    let tabuleiroPosEmpurrado = if obtemCelula tabuleiro novaLinha novaColuna /= espacoVazio
            then empurrarJogador tabuleiro (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrado (obtemCelula tabuleiro novaLinha novaColuna)
            else tabuleiro
        tabuleiroMovido = modificarTabuleiro tabuleiroPosEmpurrado novaLinha novaColuna jogadorEmpurrado
        tabuleiroAtualizado = modificarTabuleiro tabuleiroMovido linhaEmpurrado colunaEmpurrado jogadorEmpurrador
    in modificarTabuleiro tabuleiroAtualizado linhaEmpurrador colunaEmpurrador espacoVazio


-- Verifica se uma posição do tabuleiro está ocupada por algum boneco
posicaoOcupada :: Tabuleiro -> Int -> Int -> Bool
posicaoOcupada tabuleiro linha coluna =
    let valor = (tabuleiro !! linha) !! coluna 
    in valor /= espacoVazio 

--verifica se o jogador inserido está presente na coordenada de origem inserida
jogadorNaPosicao :: Tabuleiro -> Int -> Int -> String-> Bool
jogadorNaPosicao tabuleiro linha coluna jogador = 
    let valor = (tabuleiro !! linha) !! coluna
    in valor == jogador

-- Verifica vitória
verificarVitoria :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> String -> String -> (Bool, String, String)
verificarVitoria tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador1 nome1 jogador2 nome2 
    | ((existeJogador tabuleiroPassado jogador1) + (existeJogador tabuleiroPresente jogador1) + (existeJogador tabuleiroFuturo jogador1)) == 1 
        = (True, jogador2, nome2)  -- Retorna o emoji e nome do vencedor
    | ((existeJogador tabuleiroPassado jogador2) + (existeJogador tabuleiroPresente jogador2) + (existeJogador tabuleiroFuturo jogador2)) == 1 
        = (True, jogador1, nome1)  -- Retorna o emoji e nome do vencedor
    | otherwise = (False, jogador1, nome1) 

-- Verifica se existe peça de jogador em um tabuleiro específico, retorna 1 se existe, caso contrário retorna 0
existeJogador :: Tabuleiro -> String -> Int
existeJogador tabuleiro jogador
    | any (elem jogador) tabuleiro = 1
    | otherwise = 0

selecionarTabuleiro :: String -> Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro
selecionarTabuleiro "passado" tPassado _ _ = tPassado
selecionarTabuleiro "presente" _ tPresente _ = tPresente
selecionarTabuleiro "futuro" _ _ tFuturo = tFuturo

