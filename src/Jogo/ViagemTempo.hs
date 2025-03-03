module Jogo.ViagemTempo (defineViagem, posicaoLivre, viagem) where

import System.IO (hFlush, stdout)
import Utils.ImprimirTxt (imprimirTxt)
import Jogo.Tabuleiro (Tabuleiro, atualizarTabuleiroViagem, selecionarTabuleiro, espacoVazio)
import Data.Char (toLower)

viagem :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> Int -> Int -> Int -> String -> (Tabuleiro, Tabuleiro, Tabuleiro, Int)
viagem tPassado tPresente tFuturo novoTempo tempoAtual linha coluna clones jogadorAtual =
    let (novoTabuleiro, tabuleiroOrigemAtualizado) = 
            case novoTempo of
                "passado"  -> atualizarTabuleiroViagem tPassado tPresente linha coluna jogadorAtual
                "presente" -> atualizarTabuleiroViagem tPresente tPassado linha coluna jogadorAtual
                "futuro"   -> atualizarTabuleiroViagem tFuturo tPresente linha coluna jogadorAtual
    in
        if novoTempo == "passado" && tempoAtual == "presente"
            then (novoTabuleiro, tPresente, tFuturo, (clones + 1))
        else if novoTempo == "futuro" && tempoAtual == "presente"
            then (tPassado, tabuleiroOrigemAtualizado, novoTabuleiro, clones)
        else if novoTempo == "presente" && tempoAtual == "passado"
            then (tabuleiroOrigemAtualizado, novoTabuleiro, tFuturo, clones)
        else
            (tPassado, novoTabuleiro, tFuturo, (clones + 1))


-- Função usada para que o jogador escolha o tempo para o qual deseja viajar 
defineViagem :: String -> String -> Int -> IO String 
defineViagem caminhoArquivo tempoAtual clones = do 
    imprimirTxt caminhoArquivo
    hFlush stdout
    entrada <- getLine
    let entradaMinuscula = unwords . words $ map toLower entrada  -- Remove espaços extras e converte para minúsculo
    let novoTempo = case entradaMinuscula of
            "s" -> "passado"
            "p" -> "presente"
            "f" -> "futuro"
            _    -> "invalido"

    if novoTempo == tempoAtual then do 
        putStrLn "Lembre-se que não podemos viajar para o tempo que estamos atualmente"
        return "viagem impossível"
    else if (novoTempo == "passado" && tempoAtual == "futuro") || (novoTempo == "futuro" && tempoAtual == "passado") then do 
        putStrLn "Lembre-se que não podemos viajar direto do futuro para o passado ou o contrário"
        return "viagem impossível"
    -- else if novoTempo == "futuro" && tempoAtual == "passado" then do 
    --     putStrLn "Lembre-se que não podemos viajar direto do passado para o futuro"
    --     defineViagem caminhoArquivo tempoAtual
    else if (tempoAtual == "passado" && novoTempo == "presente") ||  (tempoAtual == "presente" && novoTempo == "futuro")
        then return novoTempo
    else if (tempoAtual == "presente" && novoTempo == "passado") || (tempoAtual == "futuro" && novoTempo == "presente") then do
        if clones < 4 then return novoTempo
        else do
            putStr "Não é possível viajar para o passado, pois a quantidade máxima de clones (4), já foi excedida"
            return "viagem impossível"
    -- else if tempoAtual == "presente" && novoTempo == "futuro"
    --     then return novoTempo
    -- else if tempoAtual == "futuro" && novoTempo == "presente" then do
    --     utStr "Não é possível viajar para o passado, pois a quantidade máxima de clones (4), já foi excedida"
        
    else do 
        putStrLn "Opção Inválida"
        defineViagem caminhoArquivo tempoAtual clones


-- Função usada para verificar se a posição que o jogador quer ocupar no novo tempo está ocupada
posicaoLivre :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> Int -> Int -> Bool
posicaoLivre tPassado tPresente tFuturo novoTempo linha coluna = 
    let tabuleiro = selecionarTabuleiro novoTempo tPassado tPresente tFuturo
    in (tabuleiro !! linha !! coluna) == espacoVazio -- verifica se na linha e na coluna o char é o quadrado e se for retorna true