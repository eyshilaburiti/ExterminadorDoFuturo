module Utils.Jogo (iniciarTabuleiro) where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Utils.Tabuleiro

iniciarTabuleiro :: IO ()
iniciarTabuleiro = do
    let tabuleiroPassado = tabuleiro4x4
    let tabuleiroPresente = tabuleiro4x4
    let tabuleiroFuturo = tabuleiro4x4
    jogar tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador1

-- Loop do jogo, alternando entre os jogadores
jogar :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> IO ()
jogar tPassado tPresente tFuturo jogadorAtual = do
    putStrLn "\nTabuleiros atuais:"
    imprimirTabuleiros tPassado tPresente tFuturo

    putStrLn $ "\nVez do jogador: " ++ jogadorAtual
    foco <- definirFoco
    (linha, coluna) <- obterJogada

    --usar guardas
    let tabuleiroSelecionado = case foco of
            "passado" -> tPassado
            "presente" -> tPresente
            "futuro" -> tFuturo
            _ -> tPresente -- fallback para evitar erros

    -- Verifica se a posição já está ocupada
    if (tabuleiroSelecionado !! linha !! coluna) /= "\x25A1" -- criar função pra especificar qual o emoji
        then do
            putStrLn "Posição já ocupada! Escolha outra."
            jogar tPassado tPresente tFuturo jogadorAtual
        else do
            let novoTabuleiro = atualizarTabuleiro tabuleiroSelecionado linha coluna jogadorAtual

            let (novoTPassado, novoTPresente, novoTFuturo) = case foco of
                    "passado"  -> (novoTabuleiro, tPresente, tFuturo)
                    "presente" -> (tPassado, novoTabuleiro, tFuturo)
                    "futuro"   -> (tPassado, tPresente, novoTabuleiro)
                    _          -> (tPassado, tPresente, tFuturo)

            let proximoJogador = if jogadorAtual == jogador1 then jogador2 else jogador1
            jogar novoTPassado novoTPresente novoTFuturo proximoJogador

-- Solicitar jogada do jogador
obterJogada :: IO (Int, Int)
obterJogada = do
    linha <- obterPosicao "Digite a linha (1 a 4): "
    coluna <- obterPosicao "Digite a coluna (1 a 4): "
    return (linha, coluna)
    
-- Função para obter um número válido dentro de um intervalo específico
obterPosicao :: String -> IO Int
obterPosicao mensagem = do
    putStr mensagem
    hFlush stdout
    posicao <- getLine
    case readMaybe posicao :: Maybe Int of
        Just n ->
            if n > 0 && n <= 4
                then return (n - 1)
            else do
                putStrLn "Posição inválida! Tente novamente."
                obterPosicao mensagem
        Nothing -> do
            putStrLn "Entrada inválida!"
            obterPosicao mensagem

-- Função que define o foco do jogador
definirFoco :: IO String 
definirFoco = do
    putStr "Escolha o foco para a próxima rodada (passado, presente, futuro): "
    hFlush stdout
    foco <- getLine

    if foco == "passado"
        then return foco
    else if foco == "presente"
        then return foco
    else if foco == "futuro"
        then return foco
    else do
        putStrLn "Opção Inválida"
        definirFoco  -- Chama a função novamente até que o jogador insira um valor válido.
