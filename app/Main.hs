module Main (main) where
import Utils.Jogo
import Utils.Tabuleiro

-- main :: IO ()
-- main = do
--     -- Criação de três matrizes de mesma dimensão para representar cada tabuleiro temporal
--     let matrixPassado = matrizString 4 4 "▢" -- Caso o ▢ não funcione, trocar por []
--     let matrizPresente = matrizString 4 4 "▢"
--     let matrizFuturo = matrizString 4 4 "▢"
--     startTabuleiro [matrixPassado, matrizPresente, matrizFuturo]

main :: IO ()
main = do
    let tabuleiroPassado = tabuleiro4x4
    let tabuleiroPresente = tabuleiro4x4
    let tabuleiroFuturo = tabuleiro4x4

    jogar tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador1
