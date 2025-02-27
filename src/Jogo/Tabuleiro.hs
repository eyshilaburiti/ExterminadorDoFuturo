module Jogo.Tabuleiro where

-- Criando o tabuleiro 
type Tabuleiro = [[String]]

tabuleiro4x4 :: Tabuleiro
tabuleiro4x4 = replicate 4 (replicate 4 espacoVazio) -- repetindo o símbolo do quadrado vazio ☐

-- Definindo os emojis dos jogadores
espacoVazio :: String
espacoVazio = "\x1F533"

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
    
-- Colocar função de foco aqui )para a representação gráfica no jogo)


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
        Nothing -> -- o empurrado sai do tabuleiro (morte pelas bordas)
            let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado espacoVazio
                tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemEmpurrado linhaEmpurrado colunaEmpurrado jogadorEmpurrador
            in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio
        
        Just (novaLinha, novaColuna) -> -- ele eh empurrado 
            let conteudoNovo = obtemCelula tabuleiro novaLinha novaColuna -- Caso o Empurrado Tenha uma Nova Posição
            in if conteudoNovo == jogadorEmpurrado -- Verifica se a nova posição tem uma peça do mesmo jogador
                then -- PARADOXO: elimina ambas as peças e move o empurrador
                    let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado espacoVazio  -- Remove o empurrado
                        tabuleiroSemAmbos = modificarTabuleiro tabuleiroSemEmpurrado novaLinha novaColuna espacoVazio    -- Remove a peça na nova posição
                        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemAmbos linhaEmpurrado colunaEmpurrado jogadorEmpurrador  -- Empurrador ocupa a posição do empurrado
                    in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio  -- Limpa a posição original do empurrador
                
                else if conteudoNovo == arbusto
                    then -- MORTE PELO ARBUSTO: mata a pessoa que foi para a casa do arbusto 
                    let tabuleiroSemEmpurrado = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado espacoVazio -- remove o empurrado
                        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemEmpurrado linhaEmpurrado colunaEmpurrado jogadorEmpurrador -- Empurrador ocupa a posição do empurrado
                    in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio  -- Limpa a posição original do empurrador
                
                else if conteudoNovo == arvore && jogadorEmpurrado /= arvore -- Se há uma árvore no caminho do empurrado (mas o empurrado não é uma árvore)
                    then 
                    -- O Empurrado empurra uma árvore - a árvore desaparece, e ambos os jogadores permanecem vivos
                    let 
                        -- Remove a árvore do destino
                        tabuleiroSemArvore = modificarTabuleiro tabuleiro novaLinha novaColuna espacoVazio
                        -- Move o empurrado para esta posição (onde estava a árvore)
                        tabuleiroComEmpurrado = modificarTabuleiro tabuleiroSemArvore novaLinha novaColuna jogadorEmpurrado
                        -- Empurrador ocupa a posição do empurrado
                        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroComEmpurrado linhaEmpurrado colunaEmpurrado jogadorEmpurrador
                    in modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio
                
                else if jogadorEmpurrado == arvore -- Caso o empurrado seja uma árvore
                    then 
                    -- A árvore cai e elimina qualquer jogador na posição seguinte
                    let 
                        -- Primeiro removemos a árvore da posição original
                        tabuleiroSemArvore = modificarTabuleiro tabuleiro linhaEmpurrado colunaEmpurrado espacoVazio
                        -- Movemos o empurrador para a posição da árvore
                        tabuleiroComEmpurrador = modificarTabuleiro tabuleiroSemArvore linhaEmpurrado colunaEmpurrado jogadorEmpurrador
                        -- Esvaziamos a posição original do empurrador
                        tabuleiroMovido = modificarTabuleiro tabuleiroComEmpurrador linhaEmpurrador colunaEmpurrador espacoVazio
                        -- Checamos o conteúdo da nova posição (onde a árvore cairia)
                        -- Se houver um jogador lá, ele é eliminado; a árvore também some
                    in if conteudoNovo /= espacoVazio && conteudoNovo /= arbusto && conteudoNovo /= semente
                    -- in if conteudoNovo /= espacoVazio && conteudoNovo /= arbusto -- SEM A SEMENTE

                        then 
                            -- Há um jogador na posição, eliminamos ele (árvore caiu sobre ele)
                            modificarTabuleiro tabuleiroMovido novaLinha novaColuna espacoVazio
                        else 
                            -- Não há jogador, apenas mantemos o tabuleiro como está
                            tabuleiroMovido
                
                else -- Lógica normal de empurrão (com recursão para empurrão em cadeia)
                    let tabuleiroPosEmpurrado = if conteudoNovo /= espacoVazio
                            then empurrarJogador tabuleiro (linhaEmpurrado, colunaEmpurrado) (novaLinha, novaColuna) jogadorEmpurrado conteudoNovo
                            else tabuleiro
                    in -- Agora, finalizamos movendo o empurrado:
                        let tabuleiroMovido = modificarTabuleiro tabuleiroPosEmpurrado novaLinha novaColuna jogadorEmpurrado
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
verificarVitoria :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> (Bool, String)
verificarVitoria tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador1 jogador2 
    | ((existeJogador tabuleiroPassado jogador1) + (existeJogador tabuleiroPresente jogador1) + (existeJogador tabuleiroFuturo jogador1)) == 1 = (True, jogador2)
    | ((existeJogador tabuleiroPassado jogador2) + (existeJogador tabuleiroPresente jogador2) + (existeJogador tabuleiroFuturo jogador2)) == 1 = (True, jogador1)
    | otherwise = (False, jogador1)

-- Verifica se existe peça de jogador em um tabuleiro específico, retorna 1 se existe, caso contrário retorna 0
existeJogador :: Tabuleiro -> String -> Int
existeJogador tabuleiro jogador
    | any (elem jogador) tabuleiro = 1
    | otherwise = 0

selecionarTabuleiro :: String -> Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro
selecionarTabuleiro "passado" tPassado _ _ = tPassado
selecionarTabuleiro "presente" _ tPresente _ = tPresente
selecionarTabuleiro "futuro" _ _ tFuturo = tFuturo

