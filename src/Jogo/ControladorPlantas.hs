module Jogo.ControladorPlantas (plantarSemente, removerSemente) where

import Jogo.Tabuleiro (Tabuleiro, plantarSementeNoTabuleiro,removerSementeNoTabuleiro, semente, arbusto, arvore, espacoVazio, plantaCerta, modificarTabuleiro)

-- Função para plantar semente no tabuleiro
plantarSemente :: Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> Int -> Int -> IO (Tabuleiro, Tabuleiro, Tabuleiro)
plantarSemente tabuleiroSelecionado tPassado tPresente tFuturo foco linha coluna = do
    if (foco == "passado") then do
        let novoTabuleiro = plantarSementeNoTabuleiro tabuleiroSelecionado linha coluna semente
        if(plantaCerta novoTabuleiro linha coluna semente) then do 
            let novoTabuleiroPresente = plantarSementeNoTabuleiro tPresente linha coluna arbusto
            if(plantaCerta novoTabuleiroPresente linha coluna arbusto) then do
                let novoTabuleiroFuturo = plantarSementeNoTabuleiro tFuturo linha coluna arvore
                return (novoTabuleiro, novoTabuleiroPresente, novoTabuleiroFuturo)
            else return (novoTabuleiro, novoTabuleiroPresente, tFuturo)
        else return (novoTabuleiro, tPresente, tFuturo)

    else if (foco == "presente") then do 
        let novoTabuleiro = plantarSementeNoTabuleiro tabuleiroSelecionado linha coluna semente
        if (plantaCerta novoTabuleiro linha coluna semente) then do
            let novoTabuleiroFuturo = plantarSementeNoTabuleiro tFuturo linha coluna arbusto
            return (tPassado, novoTabuleiro, novoTabuleiroFuturo)
        else return (tPassado, novoTabuleiro, tFuturo)

    else do 
        let novoTabuleiro = plantarSementeNoTabuleiro tabuleiroSelecionado linha coluna semente
        return (tPassado, tPresente, novoTabuleiro)

--Função que remove semente (e consequentemente futuras plantas) do tabuleiro
removerSemente :: Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> Int -> Int -> Int -> Int -> String -> (Tabuleiro, Tabuleiro, Tabuleiro)
removerSemente tabuleiroSelecionado tPassado tPresente tFuturo foco linhaOrigem colunaOrigem linhaDestino colunaDestino jogador =
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