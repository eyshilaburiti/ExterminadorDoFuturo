

module Jogo.Bot (escolherJogadaBot, escolherTempoBot, escolherOrigemBot, escolherDestinoBot, escolherFocoBot) where

import System.Random (randomRIO)
import Jogo.Tabuleiro (Tabuleiro, verificarJogadorTabuleiro, movimentoValido, posicaoOcupada, selecionarTabuleiro, jogadorNaPosicao)

-- | O bot escolhe uma jogada: "m" para mover, "p" para plantar, "v" para viajar
escolherJogadaBot :: IO String
escolherJogadaBot = do
    let jogadas = ["m", "p", "v"]
    indice <- randomRIO (0, length jogadas - 1) -- gera um número aleatorio da lista para escolher
    return (jogadas !! indice)

-- | O bot escolhe um tempo válido para viajar (passado, presente ou futuro)
escolherTempoBot :: String -> IO String
escolherTempoBot focoAtual = do
    let tempos = case focoAtual of
                    "passado" -> ["presente"]
                    "presente" -> ["passado", "futuro"]
                    "futuro" -> ["presente"] -- mostra as opções de viagem possível
    indice <- randomRIO (0, length tempos - 1)
    return (tempos !! indice)

-- | O bot escolhe a primeira peça válida no tabuleiro
escolherOrigemBot :: Tabuleiro -> String -> IO (Int, Int)
escolherOrigemBot tabuleiro jogador = do
    let pecas = [(linha, coluna) |
                    linha <- [0..3], coluna <- [0..3],
                    verificarJogadorTabuleiro jogador tabuleiro,
                    jogadorNaPosicao tabuleiro linha coluna jogador] -- lista todas as pecas no tabuleiro
    
    return (head pecas) -- retorna a coordenada da peça escolhida de origem

-- | O bot escolhe um destino válido ao redor da peça escolhida
escolherDestinoBot :: (Int, Int) -> IO (Int, Int)
escolherDestinoBot (linha, coluna) = do
    let destinos = destinosValidos (linha, coluna)
    indice <- randomRIO (0, length destinos - 1)
    return (destinos !! indice)

-- Gera a lista de destinos válidos ao redor de uma peça (cima, baixo, esquerda, direita)
destinosValidos :: (Int, Int) -> [(Int, Int)]
destinosValidos (linha, coluna) = 
    filter dentroDoTabuleiro [(linha + 1, coluna), (linha - 1, coluna), (linha, coluna + 1), (linha, coluna - 1)]
  where
    dentroDoTabuleiro (l, c) = l >= 0 && l < 4 && c >= 0 && c < 4


escolherFocoBot :: String -> IO String
escolherFocoBot foco = do
    let focos = ["passado", "presente", "futuro"]
    indice <- randomRIO (0, length focos - 1)
    return (focos !! indice)