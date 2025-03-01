module Interface.Jogador(jogadorNoFoco, obterJogadaOrigem, obterJogadaDestino, definirFoco, escolherJogada, escolherOpcaoMenu, exibirOpcaoMenu, jogadorNoFoco) where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Utils.ImprimirTxt (imprimirTxt)
import Jogo.Tabuleiro(jogadorNaPosicao, existeJogador, Tabuleiro)
import Data.Char (toLower)  -- Importa a função toLower para converter caracteres para minúscula

escolherJogada :: IO String
escolherJogada = do 
    imprimirTxt "src/Interface/jogadas.txt"
    hFlush stdout
    escolha <- getLine
    let escolhaMinuscula = map toLower escolha  -- Converte a entrada para minúscula
    if escolhaMinuscula == "m" then do
        return escolhaMinuscula
    else if escolhaMinuscula == "p" then do
        return escolhaMinuscula
    else if escolhaMinuscula == "v" then do
        return escolhaMinuscula
    else if escolhaMinuscula == "r" then do
        return escolhaMinuscula
    else do 
        putStrLn "Entrada inválida!"
        escolherJogada  

-- Solicitar jogada do jogador
obterJogadaOrigem :: String -> String -> Tabuleiro-> IO (Int, Int)
obterJogadaOrigem mensagem jogador tabuleiro= do
    putStrLn mensagem
    linha <- obterPosicao "Digite a linha (1 a 4): "
    coluna <- obterPosicao "Digite a coluna (1 a 4): "
    --verifica se  existe algum jogador naquela posição do tabuleiro
    if jogadorNaPosicao tabuleiro linha coluna jogador then  
        return (linha, coluna)
    else do
        putStrLn "Não existe nenhuma peça sua nessa posição do tabuleiro, escolha uma posição que já tenha uma peça sua"
        obterJogadaOrigem mensagem jogador tabuleiro

obterJogadaDestino :: String -> Int -> Int -> String -> IO (Int, Int)
obterJogadaDestino caminhoArquivo linha coluna jogador = do
    imprimirTxt caminhoArquivo
    putStr "Escolha uma opção digitando a tecla correspondente: "
    hFlush stdout
    movimento <- getLine
    if movimento == "w" then
        if linha == 0 then do
            putStrLn "Movimento inválido"
            obterJogadaDestino caminhoArquivo linha coluna jogador
        else return (linha - 1, coluna)
    else if movimento == "s" then
        if linha == 3 then do
            putStrLn "Movimento inválido"
            obterJogadaDestino caminhoArquivo linha coluna jogador
        else return (linha + 1, coluna)
    else if movimento == "a" then
        if coluna == 0 then do
            putStrLn "Movimento inválido"
            obterJogadaDestino caminhoArquivo linha coluna jogador
        else return (linha, coluna - 1)
    else if movimento == "d" then
        if coluna == 3 then do
            putStrLn "Movimento inválido"
            obterJogadaDestino caminhoArquivo linha coluna jogador
        else return (linha, coluna + 1)
    else do
        putStrLn "Comando inválido"
        obterJogadaDestino caminhoArquivo linha coluna jogador

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

-- Função para plantar semente
obterLocalSemente :: IO (Int, Int)
obterLocalSemente = do
    linha <- obterPosicao "Digite a linha onde deseja plantar a semente: (1 a 4)"
    coluna <- obterPosicao "Digite a coluna onde deseja plantar a semente: (1 a 4)"
    return (linha, coluna)

-- Função que define o foco do jogador
definirFoco :: String -> Tabuleiro -> Tabuleiro -> Tabuleiro-> String -> String -> IO String 
definirFoco caminhoArquivo tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador focoAnterior = do
    imprimirTxt caminhoArquivo
    hFlush stdout
    foco <- getLine

    case foco of
        "passado" -> 
            if (existeJogador tabuleiroPassado jogador) == 1 
                then return foco
                else do
                    putStrLn "Jogador não encontrado nesse tempo."
                    definirFoco caminhoArquivo tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador focoAnterior

        "presente" -> 
            if (existeJogador tabuleiroPresente jogador) == 1 
                then return foco
                else do
                    putStrLn "Jogador não encontrado nesse tempo."
                    definirFoco caminhoArquivo tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador focoAnterior

        "futuro" -> 
            if (existeJogador tabuleiroFuturo jogador) == 1 
                then return foco
                else do
                    putStrLn "Jogador não encontrado nesse tempo."
                    definirFoco caminhoArquivo tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador focoAnterior

        _ -> do
            putStrLn "Opção Inválida"
            definirFoco caminhoArquivo tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador focoAnterior

escolherOpcaoMenu :: IO String
escolherOpcaoMenu = do 
    imprimirTxt "src/Interface/menu.txt"
    hFlush stdout
    escolha <- getLine
    let escolhaMinuscula = map toLower escolha  -- Converte a entrada para minúscula
    if escolhaMinuscula == "d" then do
        return escolhaMinuscula
    else if escolhaMinuscula == "m" then do
        return escolhaMinuscula
    else if escolhaMinuscula == "j" then do
        return escolhaMinuscula
    else do 
        putStrLn "Entrada inválida!"
        escolherOpcaoMenu

exibirOpcaoMenu :: String -> IO ()
exibirOpcaoMenu opcao
    | opcao == "d" = imprimirTxt "src/Interface/detalhamentoJogo.txt"
    | opcao == "m" = imprimirTxt "src/Interface/detalhamentoJogo.txt"
    | opcao == "j" = putStr ""
    | otherwise = putStr ""

jogadorNoFoco :: Tabuleiro -> String -> String -> Bool
jogadorNoFoco tabuleiro foco jogador = 
    if foco `elem` ["passado", "presente", "futuro"]
        then existeJogador tabuleiro jogador == 1
        else False