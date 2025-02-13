module Jogo.MovimentarPeca (movimentarPeca) where

import Jogo.Tabuleiro (Tabuleiro, atualizarTabuleiro)

-- Modificando a assinatura da função para retornar Maybe
movimentarPeca :: Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> Int -> Int -> Int -> Int -> IO (Tabuleiro, Tabuleiro, Tabuleiro)
movimentarPeca tabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual foco linhaOrigem colunaOrigem linhaDestino colunaDestino = do
    let novoTabuleiro = atualizarTabuleiro tabuleiroSelecionado linhaOrigem colunaOrigem linhaDestino colunaDestino jogadorAtual
    return $
        if foco == "passado"
            then (novoTabuleiro, tPresente, tFuturo)
        else if foco == "presente"
            then (tPassado, novoTabuleiro, tFuturo)
        else
            (tPassado, tPresente, novoTabuleiro)