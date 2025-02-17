{--module Jogo.MovimentarPeca (movimentarPeca, verificarMorteNoArbusto) where

import Jogo.Tabuleiro (Tabuleiro, atualizarTabuleiro, movimentoValido, empurrarJogador, modificarTabuleiro)

movimentarPeca :: Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> Int -> Int -> Int -> Int -> IO (Tabuleiro, Tabuleiro, Tabuleiro)
movimentarPeca tabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual foco linhaOrigem colunaOrigem linhaDestino colunaDestino =
    let ocupante = (tabuleiroSelecionado !! linhaDestino) !! colunaDestino -- Captura quem está na posição de destino
        outroJogador = if ocupante /= "\x1F533" && ocupante /= jogadorAtual && ocupante /= "\x1F330" && ocupante /= "\x1F331" then ocupante else ""
        novoTabuleiro = if ocupante == "\x1F331" -- Se for um arbusto, o jogador morre
                        then modificarTabuleiro tabuleiroSelecionado linhaOrigem colunaOrigem "\x1F533"
                        else if outroJogador /= ""
                            then empurrarJogador tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino) jogadorAtual outroJogador
                            else modificarTabuleiro (modificarTabuleiro tabuleiroSelecionado linhaOrigem colunaOrigem "\x1F533") linhaDestino colunaDestino jogadorAtual
    in if movimentoValido tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino)
        then return $
                if foco == "passado"
                    then (novoTabuleiro, tPresente, tFuturo)
                else if foco == "presente"
                    then (tPassado, novoTabuleiro, tFuturo)
                else (tPassado, tPresente, novoTabuleiro)
        else do
            putStrLn "Movimento inválido! Você não pode se mover para essa casa."
            return (tPassado, tPresente, tFuturo)

verificarMorteNoArbusto :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
verificarMorteNoArbusto tabuleiro linha coluna jogador =
    if (tabuleiro !! linha !! coluna) == "\x1F331" -- Verifica se a casa contém um arbusto
        then modificarTabuleiro tabuleiro linha coluna "\x1F331" -- Remove o jogador, mantendo o arbusto
        else tabuleiro
        -}
module Jogo.MovimentarPeca (movimentarPeca) where

import Jogo.Tabuleiro (Tabuleiro, atualizarTabuleiro, movimentoValido, empurrarJogador, modificarTabuleiro)

movimentarPeca :: Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> Int -> Int -> Int -> Int -> IO (Tabuleiro, Tabuleiro, Tabuleiro)
movimentarPeca tabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual foco linhaOrigem colunaOrigem linhaDestino colunaDestino =
    let ocupante = (tabuleiroSelecionado !! linhaDestino) !! colunaDestino -- Captura quem está na posição de destino
        outroJogador = if ocupante /= "\x1F533" && ocupante /= jogadorAtual && ocupante /= "\x1F330" then ocupante else ""
    in if movimentoValido tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino)
        then do
            let novoTabuleiro = if outroJogador /= "" && outroJogador /= "\x1F330"
                                then empurrarJogador tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino) jogadorAtual outroJogador
                                else modificarTabuleiro (modificarTabuleiro tabuleiroSelecionado linhaOrigem colunaOrigem "\x1F533") linhaDestino colunaDestino jogadorAtual
            return $
                if foco == "passado"
                    then (novoTabuleiro, tPresente, tFuturo)
                else if foco == "presente"
                    then (tPassado, novoTabuleiro, tFuturo)
                else (tPassado, tPresente, novoTabuleiro)
        else do
            putStrLn "Movimento inválido! Você não pode se mover para essa casa."
            return (tPassado, tPresente, tFuturo)