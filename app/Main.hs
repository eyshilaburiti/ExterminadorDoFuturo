module Main (main) where
import Utils.Jogo
import Utils.Tabuleiro

main :: IO ()
main = do
    let tabuleiroPassado = tabuleiro4x4
    let tabuleiroPresente = tabuleiro4x4
    let tabuleiroFuturo = tabuleiro4x4

    jogar tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador1
