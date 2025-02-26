module Jogo.MovimentarPeca (movimentarPeca) where

import Jogo.Tabuleiro (Tabuleiro, atualizarTabuleiro, movimentoValido, empurrarJogador, modificarTabuleiro, semente, removerSementeNoTabuleiro, obtemCelula, arbusto, espacoVazio)

movimentarPeca :: Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> Int -> Int -> Int -> Int -> IO (Tabuleiro, Tabuleiro, Tabuleiro, Bool)
movimentarPeca tabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual foco linhaOrigem colunaOrigem linhaDestino colunaDestino =
    let ocupante = obtemCelula tabuleiroSelecionado linhaDestino colunaDestino
    in if movimentoValido tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino)
        then do
            let (novoTabuleiro, jogadorMorreu) 
                    | ocupante == arbusto =
                        -- Morte: remove a peça que entrou na planta
                        (modificarTabuleiro tabuleiroSelecionado linhaOrigem colunaOrigem espacoVazio, True)
                    | ocupante /= espacoVazio && ocupante /= semente && ocupante /= jogadorAtual = 
                        -- Empurra jogadores, ignora plantas
                        (empurrarJogador tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino) jogadorAtual ocupante, False)
                    | otherwise = 
                        -- Movimento normal
                        (modificarTabuleiro (modificarTabuleiro tabuleiroSelecionado linhaOrigem colunaOrigem espacoVazio) linhaDestino colunaDestino jogadorAtual, False)
            let (novoTPassado, novoTPresente, novoTFuturo) =
                    if foco == "passado" 
                        then (novoTabuleiro, tPresente, tFuturo)
                    else if foco == "presente"
                        then (tPassado, novoTabuleiro, tFuturo)
                    else (tPassado, tPresente, novoTabuleiro)

            return (novoTPassado, novoTPresente, novoTFuturo, jogadorMorreu)
        else do
            putStrLn "Movimento inválido! Você não pode se mover para essa casa."
            return (tPassado, tPresente, tFuturo, False)
