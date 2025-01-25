module Main (main) where
import Utils.Matriz

main :: IO ()
main = do
    -- Criação de três matrizes de mesma dimensão para representar cada tabuleiro temporal
    let matrixPassado = matrizString 4 4 "▢" -- Caso o ▢ não funcione, trocar por []
    let matrizPresente = matrizString 4 4 "▢"
    let matrizFuturo = matrizString 4 4 "▢"
    startTabuleiro [matrixPassado, matrizPresente, matrizFuturo]