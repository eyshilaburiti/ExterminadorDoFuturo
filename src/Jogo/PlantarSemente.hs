module Jogo.PlantarSemente (plantarSemente) where

import Jogo.Tabuleiro (Tabuleiro, plantarSementeNoTabuleiro, semente, arbusto, arvore, temPlanta)


plantarSemente :: Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> Int -> Int -> IO (Tabuleiro, Tabuleiro, Tabuleiro)

plantarSemente tabuleiroSelecionado tPassado tPresente tFuturo foco linha coluna = do
    if (foco == "passado") then do
        let novoTabuleiro = plantarSementeNoTabuleiro tabuleiroSelecionado linha coluna semente
        if(temPlanta novoTabuleiro linha coluna) then do 
            let novoTabuleiroPresente = plantarSementeNoTabuleiro tPresente linha coluna arbusto
            if(temPlanta novoTabuleiroPresente linha coluna) then do
                let novoTabuleiroFuturo = plantarSementeNoTabuleiro tFuturo linha coluna arvore
                return (novoTabuleiro, novoTabuleiroPresente, novoTabuleiroFuturo)
            else return (novoTabuleiro, novoTabuleiroPresente, tFuturo)
        else return (novoTabuleiro, tPresente, tFuturo)

    else if (foco == "presente") then do 
        let novoTabuleiro = plantarSementeNoTabuleiro tabuleiroSelecionado linha coluna semente
        if (temPlanta novoTabuleiro linha coluna) then do
            let novoTabuleiroFuturo = plantarSementeNoTabuleiro tFuturo linha coluna arbusto
            return (tPassado, novoTabuleiro, novoTabuleiroFuturo)
        else return (tPassado, novoTabuleiro, tFuturo)

    else do 
        let novoTabuleiro = plantarSementeNoTabuleiro tabuleiroSelecionado linha coluna semente
        return (tPassado, tPresente, novoTabuleiro)
    

-- if(foco == "passado") then do 
--     let novoTabuleiro = plantarSementeNoTabuleiro tabuleiroSelecionado linha coluna semente
--     let novoTabuleiroPresente = plantarSementeNoTabuleiro tPresente linha coluna arbusto
--     let novoTabuleiroFuturo = plantarSementeNoTabuleiro tFuturo linha coluna arvore
    
--     jogar novoTabuleiro novoTabuleiroPresente novoTabuleiroFuturo proximoJogador

-- else if(foco == "presente") then do 
--     let novoTabuleiro = plantarSementeNoTabuleiro tabuleiroSelecionado linha coluna semente
--     let novoTabuleiroFuturo = plantarSementeNoTabuleiro tFuturo linha coluna arbusto

--     jogar tPassado novoTabuleiro novoTabuleiroFuturo proximoJogador

-- else if(foco == "futuro") then do 
--     let novoTabuleiro = plantarSementeNoTabuleiro tabuleiroSelecionado linha coluna semente

--     jogar tPassado tPresente novoTabuleiro proximoJogador

-- else jogar tPassado tPresente tFuturo proximoJogador