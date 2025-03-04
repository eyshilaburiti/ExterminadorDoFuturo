module Utils.Bot (escolherJogadaBot, escolherTempoBot, escolherOrigemBot, escolherDestinoBot, escolherFocoBot) where

import System.Random (randomRIO)
import Jogo.Tabuleiro (Tabuleiro, verificarJogadorTabuleiro, jogadorNaPosicao, existeJogador, obtemCelula, arbusto, arvore, espacoVazio, novaPosicaoEmpurrado)

-- Função para que o bot escolha uma jogada
escolherJogadaBot :: Tabuleiro -> (Int, Int) -> String -> IO String
escolherJogadaBot tabuleiro (linha, coluna) jogador = do
    if jogadaPrioritariaBot tabuleiro (linha, coluna) jogador
        then do
            return "m"
        else do
            -- escolhe aleatoriamente 
            let jogadas = ["m", "p", "v"] 
            indice <- randomRIO (0, length jogadas - 1)
            return (jogadas !! indice)

-- Caso exista uma árvore ou um jogador perto do bot então será classificada como jogada prioriátia 
jogadaPrioritariaBot :: Tabuleiro -> (Int, Int) -> String -> Bool
jogadaPrioritariaBot tabuleiro (linha, coluna) jogador =
    let destinos = destinosValidos (linha, coluna)
        -- Checa se há uma árvore ou um oponente nos destinos adjacentes
        temArvoreOuOponente = any (\(l, c) -> 
            let celula = obtemCelula tabuleiro l c
            in celula == arvore || (celula /= espacoVazio && celula /= jogador && celula /= arbusto)) destinos
    in temArvoreOuOponente



-- Escolhe o tempo par o bot viajar 
escolherTempoBot :: String -> IO String
escolherTempoBot focoAtual = do
    -- lista as opções de mudança de tempo
    let tempos = case focoAtual of
                    "passado" -> ["presente"]
                    "presente" -> ["passado", "futuro"]
                    "futuro" -> ["presente"] 
                    _        -> []
    indice <- randomRIO (0, length tempos - 1)
    return (tempos !! indice)

-- O bot seleciona a origem escolhendo a primeira peça válida no tabuleiro
escolherOrigemBot :: Tabuleiro -> String -> IO (Int, Int)
escolherOrigemBot tabuleiro jogador = do
    let pecas = [(linha, coluna) |
                    linha <- [0..3], coluna <- [0..3],
                    verificarJogadorTabuleiro jogador tabuleiro,
                    jogadorNaPosicao tabuleiro linha coluna jogador] 
    case pecas of 
        (x:_) -> return x 
        [] -> do 
            putStr "Nenhuma peça encontrada"
            return (-1, -1) 

-- Escolhe o destino do bot
escolherDestinoBot :: Tabuleiro -> (Int, Int) -> String -> IO (Int, Int)
escolherDestinoBot tabuleiro (linhaOrigem, colunaOrigem) jogador = do
    let destinos = destinosValidos (linhaOrigem, colunaOrigem)
    let destinosMortaisDisponiveis = destinosMortais tabuleiro (linhaOrigem, colunaOrigem) jogador destinos

    if not (null destinosMortaisDisponiveis)
        then do
            indice <- randomRIO (0, length destinosMortaisDisponiveis - 1)
            return (destinosMortaisDisponiveis !! indice)
        else do
            indice <- randomRIO (0, length destinos - 1)
            return (destinos !! indice)


-- Destinos válidos ao redor da peça (cima, baixo, esquerda, direita)
destinosValidos :: (Int, Int) -> [(Int, Int)]
destinosValidos (linha, coluna) = 
    filter dentroDoTabuleiro [(linha + 1, coluna), (linha - 1, coluna), (linha, coluna + 1), (linha, coluna - 1)]

escolherFocoBot :: Tabuleiro -> Tabuleiro -> Tabuleiro-> String -> String -> IO String 
escolherFocoBot tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador foco = do
    let focos = ["passado", "presente", "futuro"]
    indice <- randomRIO (0, length focos - 1)

    case (focos !! indice) of
        "passado" -> 
            if (existeJogador tabuleiroPassado jogador) == 1 
                then return (focos !! indice)
                else do
                    putStrLn "Jogador não encontrado nesse tempo."
                    escolherFocoBot tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador foco

        "presente" -> 
            if (existeJogador tabuleiroPresente jogador) == 1 
                then return (focos !! indice)
                else do
                    putStrLn "Jogador não encontrado nesse tempo."
                    escolherFocoBot tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador foco

        "futuro" -> 
            if (existeJogador tabuleiroFuturo jogador) == 1 
                then return (focos !! indice)
                else do
                    putStrLn "Jogador não encontrado nesse tempo."
                    escolherFocoBot tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador foco

        _ -> do
            putStrLn "Opção Inválida"
            escolherFocoBot tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador foco


-- Verifica se algum dos possíveis movimentos levam a morte 
movimentoLevaMorte :: Tabuleiro -> (Int, Int) -> (Int, Int) -> String -> Bool
movimentoLevaMorte tabuleiro (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino) jogador =
    let posicaoDestino = obtemCelula tabuleiro linhaDestino colunaDestino
    in case novaPosicaoEmpurrado (linhaDestino, colunaDestino) (linhaOrigem, colunaOrigem) of
            Nothing -> posicaoDestino /= espacoVazio && posicaoDestino /= jogador -- Empurrar para fora do tabuleiro
            Just (linhaEmpurrado, colunaEmpurrado) ->
                let ocupanteEmpurrado = obtemCelula tabuleiro linhaEmpurrado colunaEmpurrado
                in
                    -- Matar ao empurrar jogador no arbusto
                    (posicaoDestino /= espacoVazio && posicaoDestino /= jogador && posicaoDestino /= arvore && posicaoDestino /= arbusto && ocupanteEmpurrado == arbusto)
                
                    -- Matar ao empurrar árvore em um jogador
                    || (posicaoDestino == arvore && ocupanteEmpurrado /= espacoVazio && ocupanteEmpurrado /= arvore && ocupanteEmpurrado /= jogador)
                
                    -- Paradoxo: empurrar um jogador contra outro jogador
                    || (posicaoDestino /= espacoVazio && posicaoDestino /= jogador &&
                        ocupanteEmpurrado /= espacoVazio && ocupanteEmpurrado /= jogador)

-- Seleciona destinos letais para o bot
destinosMortais :: Tabuleiro -> (Int, Int) -> String -> [(Int, Int)] -> [(Int, Int)]
destinosMortais tabuleiro (linha, coluna) jogador  destinosValidosLista=
    filter (\destino -> movimentoLevaMorte tabuleiro (linha, coluna) destino jogador) destinosValidosLista



dentroDoTabuleiro :: (Int, Int) -> Bool
dentroDoTabuleiro (linha, coluna) = linha >= 0 && linha < 4 && coluna >= 0 && coluna < 4