module Jogo.RemoveSemente (removeSemente) where

import Jogo.Tabuleiro (Tabuleiro, removerSementeNoTabuleiro, posicaoOcupada, espacoVazio)

removeSemente :: Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> Int -> Int -> String -> IO (Tabuleiro, Tabuleiro, Tabuleiro)

removeSemente tabuleiroSelecionado tPassado tPresente tFuturo foco linha coluna jogador = do
    if (foco == "passado") then do
        let novoTabuleiro = removerSementeNoTabuleiro tabuleiroSelecionado linha coluna espacoVazio
        let novoTabuleiroPresente = removerSementeNoTabuleiro tPresente linha coluna espacoVazio
        let novoTabuleiroFuturo = removerSementeNoTabuleiro tFuturo linha coluna espacoVazio
        return (novoTabuleiro, novoTabuleiroPresente, novoTabuleiroFuturo)

    else if (foco == "presente") then do 
        let novoTabuleiro = removerSementeNoTabuleiro tabuleiroSelecionado linha coluna jogador
        let novoTabuleiroFuturo = removerSementeNoTabuleiro tFuturo linha coluna espacoVazio
        return (tPassado, novoTabuleiro, novoTabuleiroFuturo)

    else do 
        let novoTabuleiro = removerSementeNoTabuleiro tabuleiroSelecionado linha coluna jogador

        return (tPassado, tPresente, novoTabuleiro)