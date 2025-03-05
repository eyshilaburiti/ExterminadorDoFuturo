module Jogo.MovimentarPeca (movimentarPeca) where

import Jogo.Tabuleiro (Tabuleiro, movimentoValido, empurrarJogador, modificarTabuleiro, semente, obtemCelula, arbusto, espacoVazio)
import Jogo.ControladorPlantas (removerSemente)

-- Retorna uma ação IO que produz 3 tabuleiros atualizados e um Bool indicando se o jogador morreu.
movimentarPeca :: Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> Int -> Int -> Int -> Int -> IO (Tabuleiro, Tabuleiro, Tabuleiro, Bool)
movimentarPeca tabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual foco linhaOrigem colunaOrigem linhaDestino colunaDestino =
    let ocupante = obtemCelula tabuleiroSelecionado linhaDestino colunaDestino -- obtém quem está na posição de destino.
    in if movimentoValido tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino)
        then do
            -- Tratamento da semente
            let ((novoPassado, novoPresente, novoFuturo), removeuSemente)
                    | ocupante == semente =
                        (removerSemente tabuleiroSelecionado tPassado tPresente tFuturo foco linhaOrigem colunaOrigem linhaDestino colunaDestino jogadorAtual, True)
                    | otherwise = ((tPassado, tPresente, tFuturo), False)

            let (novoTabuleiro, jogadorMorreu) 
                    | ocupante == arbusto =
                        -- Morte: remove a peça que entrou na planta
                        (modificarTabuleiro tabuleiroSelecionado linhaOrigem colunaOrigem espacoVazio, True)
                    | ocupante /= espacoVazio && ocupante /= semente && ocupante /= jogadorAtual = 
                        -- Empurra jogadores 
                        (empurrarJogador tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino) jogadorAtual ocupante, False)
                    | otherwise = 
                        -- Movimento normal: o jogador se move, ficando o seu espaço de origem vazio 
                        (modificarTabuleiro (modificarTabuleiro tabuleiroSelecionado linhaOrigem colunaOrigem espacoVazio) linhaDestino colunaDestino jogadorAtual, False)
            
            -- O tabuleiro é atualizado com base nas mudanças anteriores e com base no foco escolhido 
            let (novoTPassado, novoTPresente, novoTFuturo) =
                    if removeuSemente
                        then -- Lógica específica quando a semente é removida
                            (novoPassado, novoPresente, novoFuturo)
                        else -- Lógica normal quando a semente não é removida
                            if foco == "passado" -- Atualiza o passado 
                                then (novoTabuleiro, tPresente, tFuturo)
                            else if foco == "presente" -- Atualiza o presente
                                then (tPassado, novoTabuleiro, tFuturo)
                            else (tPassado, tPresente, novoTabuleiro) -- Atualiza o futuro
            return (novoTPassado, novoTPresente, novoTFuturo, jogadorMorreu)
        else do
            putStrLn "Movimento inválido! Você não pode se mover para essa casa."
            return (tPassado, tPresente, tFuturo, False)
