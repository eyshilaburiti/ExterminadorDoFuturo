module Jogo.Tabuleiro (Tabuleiro, imprimirTabuleiros, jogador1, jogador2, tabuleiro4x4, movimentoValido, verificarJogadorTabuleiro, verificarVitoria, posicaoOcupada,
selecionarTabuleiro, jogadorNaPosicao, existeJogador, obtemCelula, arbusto, arvore, espacoVazio, caveira, exclamacao, negado, 
novaPosicaoEmpurrado, atualizarTabuleiroViagem, atualizarTabuleiro, empurrarJogador, modificarTabuleiro, 
semente, removerSementeNoTabuleiro, plantarSementeNoTabuleiro, temPlanta)
where

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

caveira :: String
caveira = "\x1F480"

negado :: String
negado = "\x274C"

exclamacao :: String
exclamacao = "\x2757"

-- Função para formatar o tabuleiro como uma String 
formatarTabuleiro :: Tabuleiro -> [String]
formatarTabuleiro tabuleiro = map (\linha -> "|" ++ unwords linha ++ "|") tabuleiro

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
    -- modifica o tabuleiro para que o personagem seja removido dele 
    let tabuleiroOrigemAtualizado = modificarTabuleiro tabuleiroOrigem linha coluna espacoVazio 
        -- Coloca o jogador na mesma linha e coluna em que estava no tabuleiro anterior
        tabuleiroDestinoAtualizado = modificarTabuleiro tabuleiroDestino linha coluna jogador 
    -- retorna os dois tabuleiros agora atualizados
    in (tabuleiroDestinoAtualizado, tabuleiroOrigemAtualizado) 

-- Atualiza o tabuleiro com as jogadas
modificarTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
modificarTabuleiro tabuleiro linha coluna jogador = 
    take linha tabuleiro ++ [take coluna (tabuleiro !! linha) ++ [jogador] ++ drop (coluna +  1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro

-- Determina se o movimento é válido (True) ou não (False)
movimentoValido :: Tabuleiro -> (Int, Int) -> (Int, Int) -> Bool
movimentoValido tabuleiro (linhaAntiga, colunaAntiga) (linhaNova, colunaNova) =
    -- Calcula a diferença absoluta entre as coordenadas antigas e novas.
    let difLinha = abs (linhaNova - linhaAntiga)
        difColuna = abs (colunaNova - colunaAntiga)
        destinoLivre = not (posicaoOcupada tabuleiro linhaNova colunaNova) -- Verifica se o destino está vazio.
        ocupante = (tabuleiro !! linhaNova) !! colunaNova -- retorna o valor exato na posição.
    in (destinoLivre || ocupante /= espacoVazio) &&  -- Permite movimentação normal ou empurrão
       ((difLinha == 1 && difColuna == 0) || (difLinha == 0 && difColuna == 1)) -- Apenas permite movimento vertical e/ou movimento horizontal

-- Verifica se uma posição do tabuleiro está ocupada por algum elemento
posicaoOcupada :: Tabuleiro -> Int -> Int -> Bool
posicaoOcupada tabuleiro linha coluna =
    let valor = (tabuleiro !! linha) !! coluna 
    in valor /= espacoVazio 

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

verificarJogadorTabuleiro :: String -> Tabuleiro -> Bool
verificarJogadorTabuleiro jogador tabuleiro =
    any (any (== jogador)) tabuleiro

-- Empurra o elemento e retorna o tabuleiro após o empurrão
empurrarJogador :: Tabuleiro -> (Int, Int) -> (Int, Int) -> String -> String -> Tabuleiro
empurrarJogador tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador jogadorEmpurrado =
    case novaPosicaoEmpurrado (linhaEmpurrado, colunaEmpurrado) (linhaEmpurrador, colunaEmpurrador) of
         -- o empurrado for empurrado para fora do tabuleiro
        Nothing -> morteBordas tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador
        -- o empurrado for movido para uma nova posição válida dentro do tabuleiro
        Just (novaLinha, novaColuna) -> empurrao tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador jogadorEmpurrado 

-- Calcula a nova posição do jogador empurrado
novaPosicaoEmpurrado :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
novaPosicaoEmpurrado (linhaEmpurrado, colunaEmpurrado) (linhaEmpurrador, colunaEmpurrador) =
    let direcao = (linhaEmpurrado - linhaEmpurrador, colunaEmpurrado - colunaEmpurrador) -- Calcula a direção do empurrão como um vetor de deslocamento (Δlinha, Δcoluna)
        novaLinha = linhaEmpurrado + fst direcao -- fst pega o 1 elem da tupla
        novaColuna = colunaEmpurrado + snd direcao -- snd pega o 2 elem da tupla
    in if novaLinha < 0 || novaLinha >= 4 || novaColuna < 0 || novaColuna >= 4
        then Nothing  -- Se o jogador empurrado for para fora do tabuleiro, ele morre
        else Just (novaLinha, novaColuna) -- Se a posição for válida, retorna a nova posição dentro do Just.

-- Função para tratar a morte do jogador ao ser empurrado para fora do tabuleiro
morteBordas :: Tabuleiro -> (Int, Int) -> (Int, Int) -> String -> Tabuleiro
morteBordas tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador =
    -- coloca a string vazia na posição onde o empurrado estava
    let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado espacoVazio 
    -- o empurrador ocupa a posicao do empurrado
        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemEmpurrado linhaEmpurrado colunaEmpurrado jogadorEmpurrador 
    -- posição original do empurrador é substituida por um espaço vazio 
    in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio

-- Obtém a string do elemento que está na posição desejada
obtemCelula :: Tabuleiro -> Int -> Int -> String
obtemCelula tabuleiro linha coluna = (tabuleiro !! linha) !! coluna

-- Função para processar o empurrão, considerando vários casos
empurrao :: Tabuleiro -> (Int, Int) -> (Int, Int) -> (Int, Int) -> String -> String -> Tabuleiro
empurrao tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador jogadorEmpurrado =
    let conteudoNovo = obtemCelula tabuleiro novaLinha novaColuna -- obtém o elemento que está na posição desejada 
    in case conteudoNovo of
        _ | conteudoNovo == jogadorEmpurrado -> paradoxo tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador
          | conteudoNovo == arbusto -> mortePorArbusto tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador
          | conteudoNovo == arvore && jogadorEmpurrado /= arvore -> empurraArvore tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador jogadorEmpurrado
          | jogadorEmpurrado == arvore -> arvoreCai tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador
          | otherwise -> empurraoNormal tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador jogadorEmpurrado

-- Caso da morte por paradoxo: elimina ambas as peças
paradoxo :: Tabuleiro -> (Int, Int) -> (Int, Int) -> (Int, Int) -> String -> Tabuleiro
paradoxo tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador =
    let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado espacoVazio -- Remove o empurrado
        tabuleiroSemAmbos = modificarTabuleiro tabuleiroSemEmpurrado novaLinha novaColuna espacoVazio -- Remove a peça na nova posição
        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemAmbos linhaEmpurrado colunaEmpurrado jogadorEmpurrador -- Empurrador ocupa a posição do empurrado
    in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio -- Limpa a posição original do empurrador
                
-- Caso de morte por arbusto
mortePorArbusto :: Tabuleiro -> (Int, Int) -> (Int, Int) -> String -> Tabuleiro
mortePorArbusto tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) jogadorEmpurrador =
    let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado espacoVazio  -- Remove o empurrado
        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemEmpurrado linhaEmpurrado colunaEmpurrado jogadorEmpurrador -- Empurrador ocupa a posição do empurrado
    in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio -- Limpa a posição original do empurrador

-- Caso onde um jogador empurra uma árvore
empurraArvore :: Tabuleiro -> (Int, Int) -> (Int, Int) -> (Int, Int) -> String -> String -> Tabuleiro
empurraArvore tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador jogadorEmpurrado =
    let tabuleiroSemArvore = modificarTabuleiro tabuleiro novaLinha novaColuna espacoVazio -- Remove a árvore do destino
        -- Move o empurrado para esta posição (onde estava a árvore)
        tabuleiroComEmpurrado = modificarTabuleiro tabuleiroSemArvore novaLinha novaColuna jogadorEmpurrado 
         -- Empurrador ocupa a posição do empurrado
        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroComEmpurrado linhaEmpurrado colunaEmpurrado jogadorEmpurrador
    in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio

-- Caso onde uma árvore cai e pode matar um jogador
arvoreCai :: Tabuleiro -> (Int, Int) -> (Int, Int) -> (Int, Int) -> String -> Tabuleiro
arvoreCai tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador =
    let tabuleiroSemArvore = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado espacoVazio -- Remove a árvore da posição original
        -- Movem o empurrador para a posição da árvore
        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemArvore linhaEmpurrado colunaEmpurrado jogadorEmpurrador
         -- Esvazia a posição original do empurrador
        tabuleiroMovido = modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio
        -- Verifica o conteúdo da nova posição (onde a árvore cairia)
        -- Se houver um jogador lá, ele é eliminado; a árvore também some
        conteudoNovo = obtemCelula tabuleiro novaLinha novaColuna
    in if conteudoNovo /= espacoVazio && conteudoNovo /= arbusto && conteudoNovo /= semente
        -- Há um jogador na posição, ele é eliminado (árvore caiu sobre ele)
        then modificarTabuleiro tabuleiroMovido novaLinha novaColuna espacoVazio
        -- Não há jogador, apenas mantém o tabuleiro como está
        else tabuleiroMovido

-- Caso normal de empurrão
empurraoNormal :: Tabuleiro -> (Int, Int) -> (Int, Int) -> (Int, Int) -> String -> String -> Tabuleiro
empurraoNormal tabuleiro (linhaEmpurrador, colunaEmpurrador) (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrador jogadorEmpurrado =
    let tabuleiroPosEmpurrado = if obtemCelula tabuleiro novaLinha novaColuna /= espacoVazio
            then empurrarJogador tabuleiro (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrado (obtemCelula tabuleiro novaLinha novaColuna)
            else tabuleiro
        -- o jogador empurrado na nova posição
        tabuleiroMovido = modificarTabuleiro tabuleiroPosEmpurrado novaLinha novaColuna jogadorEmpurrado
        -- o jogador empurrado na posição em que o empurrado estava 
        tabuleiroAtualizado = modificarTabuleiro tabuleiroMovido linhaEmpurrado colunaEmpurrado jogadorEmpurrador
        -- coloca como vazia a posição original do empurrador
    in modificarTabuleiro tabuleiroAtualizado linhaEmpurrador colunaEmpurrador espacoVazio
        

--verifica se o jogador inserido está presente na coordenada de origem inserida
jogadorNaPosicao :: Tabuleiro -> Int -> Int -> String-> Bool
jogadorNaPosicao tabuleiro linha coluna jogador = 
    let valor = (tabuleiro !! linha) !! coluna
    in valor == jogador

-- Verifica vitória
verificarVitoria :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> String -> String -> (Bool, String, String)
verificarVitoria tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador1' nome1 jogador2' nome2 
    | ((existeJogador tabuleiroPassado jogador1') + (existeJogador tabuleiroPresente jogador1') + (existeJogador tabuleiroFuturo jogador1')) == 1 
        = (True, jogador2', nome2)  -- Retorna o emoji e nome do vencedor
    | ((existeJogador tabuleiroPassado jogador2') + (existeJogador tabuleiroPresente jogador2') + (existeJogador tabuleiroFuturo jogador2')) == 1 
        = (True, jogador1', nome1)  -- Retorna o emoji e nome do vencedor
    | otherwise = (False, jogador1', nome1) 

-- Verifica se existe peça de jogador em um tabuleiro específico, retorna 1 se existe, caso contrário retorna 0
existeJogador :: Tabuleiro -> String -> Int
existeJogador tabuleiro jogador
    | any (elem jogador) tabuleiro = 1
    | otherwise = 0

selecionarTabuleiro :: String -> Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro
selecionarTabuleiro "passado" tPassado _ _ = tPassado
selecionarTabuleiro "presente" _ tPresente _ = tPresente
selecionarTabuleiro "futuro" _ _ tFuturo = tFuturo
selecionarTabuleiro _ _ _ _ = error "Tempo inválido: escolha entre 'passado', 'presente' ou 'futuro'."
