module Jogo.MovimentarPeca (movimentarPeca, removeSemente) where

import Jogo.Tabuleiro (Tabuleiro, atualizarTabuleiro, movimentoValido, empurrarJogador, modificarTabuleiro, semente, arvore, removerSementeNoTabuleiro, obtemCelula, arbusto, espacoVazio, posicaoOcupada)

movimentarPeca :: Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> Int -> Int -> Int -> Int -> IO (Tabuleiro, Tabuleiro, Tabuleiro, Bool)
movimentarPeca tabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual foco linhaOrigem colunaOrigem linhaDestino colunaDestino =
    let ocupante = obtemCelula tabuleiroSelecionado linhaDestino colunaDestino
    in if movimentoValido tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino)
        then do
            -- Tratamento da semente
            let ((novoPassado, novoPresente, novoFuturo), removeuSemente)
                    | ocupante == semente =
                        (removeSemente tabuleiroSelecionado tPassado tPresente tFuturo foco linhaOrigem colunaOrigem linhaDestino colunaDestino jogadorAtual, True)
                    | otherwise = ((tPassado, tPresente, tFuturo), False)

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
                    if removeuSemente
                        then -- Lógica específica quando a semente é removida
                            (novoPassado, novoPresente, novoFuturo)
                        else -- Lógica normal quando a semente não é removida
                            if foco == "passado"
                                then (novoTabuleiro, tPresente, tFuturo)
                            else if foco == "presente"
                                then (tPassado, novoTabuleiro, tFuturo)
                            else (tPassado, tPresente, novoTabuleiro)

            return (novoTPassado, novoTPresente, novoTFuturo, jogadorMorreu)
        else do
            putStrLn "Movimento inválido! Você não pode se mover para essa casa."
            return (tPassado, tPresente, tFuturo, False)

removeSemente :: Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> Int -> Int -> Int -> Int -> String -> (Tabuleiro, Tabuleiro, Tabuleiro)
removeSemente tabuleiroSelecionado tPassado tPresente tFuturo foco linhaOrigem colunaOrigem linhaDestino colunaDestino jogador =
    if foco == "passado"
        then
            let novoTabuleiro = modificarTabuleiro (modificarTabuleiro tabuleiroSelecionado linhaOrigem colunaOrigem espacoVazio) linhaDestino colunaDestino jogador
                novoTabuleiroPresente = removerSementeNoTabuleiro tPresente linhaDestino colunaDestino arbusto
                novoTabuleiroFuturo = removerSementeNoTabuleiro tFuturo linhaDestino colunaDestino arvore
            in (novoTabuleiro, novoTabuleiroPresente, novoTabuleiroFuturo)

        else if foco == "presente"
            then
                let novoTabuleiro = modificarTabuleiro (modificarTabuleiro tabuleiroSelecionado linhaOrigem colunaOrigem espacoVazio) linhaDestino colunaDestino jogador
                    novoTabuleiroFuturo = removerSementeNoTabuleiro tFuturo linhaDestino colunaDestino arbusto
                in (tPassado, novoTabuleiro, novoTabuleiroFuturo)

            else
                let novoTabuleiro = modificarTabuleiro (modificarTabuleiro tabuleiroSelecionado linhaOrigem colunaOrigem espacoVazio) linhaDestino colunaDestino jogador
                in (tPassado, tPresente, novoTabuleiro)