module Jogo.MovimentarPeca (movimentarPeca) where
import Jogo.Tabuleiro (Tabuleiro, atualizarTabuleiro, movimentoValido,empurrarJogador)

movimentarPeca :: Tabuleiro -> Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> Int -> Int -> Int -> Int -> IO (Tabuleiro, Tabuleiro, Tabuleiro) 
movimentarPeca tabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual foco linhaOrigem colunaOrigem linhaDestino colunaDestino =
    let ocupante = (tabuleiroSelecionado !! linhaDestino) !! colunaDestino -- Captura quem está na posição de destino
        outroJogador = if ocupante /= "\x1F533" && ocupante /= jogadorAtual then ocupante else "" -- se for algum ocupante ou um quadrado vazio (outroJogador = str vazia)
    in if movimentoValido tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino)
        then do
            let novoTabuleiro = if outroJogador /= "" 
                                then empurrarJogador tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino) jogadorAtual outroJogador
                                else atualizarTabuleiro tabuleiroSelecionado linhaOrigem colunaOrigem linhaDestino colunaDestino jogadorAtual
            return $
                if foco == "passado"
                    then (novoTabuleiro, tPresente, tFuturo)
                    else if foco == "presente"
                        then (tPassado, novoTabuleiro, tFuturo)
                        else (tPassado, tPresente, novoTabuleiro)
        else do
            putStrLn "Movimento inválido! Você não pode se mover para essa casa."
            return (tPassado, tPresente, tFuturo)  -- Mantém o tabuleiro inalterado


