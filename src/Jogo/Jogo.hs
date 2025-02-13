module Jogo.Jogo where
    
import Jogo.Tabuleiro (Tabuleiro, imprimirTabuleiros, jogador1, jogador2, tabuleiro4x4, inicializarTabuleiro, movimentoValido, verificarJogadorTabuleiro)
import Interface.Jogador (obterJogada, definirFoco, escolherJogada)
import Jogo.MovimentarPeca (movimentarPeca)

iniciarTabuleiro :: IO ()
iniciarTabuleiro = do
    let tabuleiro1 = inicializarTabuleiro tabuleiro4x4 0 0 jogador1
    let tabuleiro = inicializarTabuleiro tabuleiro1 3 3 jogador2

    let tabuleiroPassado = tabuleiro
    let tabuleiroPresente = tabuleiro
    let tabuleiroFuturo = tabuleiro

    rodadaJogador tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador1 "passado" "futuro"

rodadaJogador :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> String-> IO()
rodadaJogador tPassado tPresente tFuturo jogadorAtual focoJogador1 focoJogador2= do
    let foco = if jogadorAtual == jogador1 then focoJogador1 else focoJogador2

    putStr ("Seu foco na rodada atual é o " ++ foco)

    putStrLn $ "\nTurno do jogador: " ++ jogadorAtual
    (novoTPassado1, novoTPresente1, novoTFuturo1) <- jogar tPassado tPresente tFuturo jogadorAtual foco
    (novoTPassado2, novoTPresente2, novoTFuturo2) <- jogar novoTPassado1 novoTPresente1 novoTFuturo1 jogadorAtual foco
    
    imprimirTabuleiros novoTPassado2 novoTPresente2 novoTFuturo2

    novoFoco <- definirFoco "src/Interface/foco.txt" foco

    let (novoFocoJogador1, novoFocoJogador2) =
            if jogadorAtual == jogador1
                then (novoFoco, focoJogador2)
                else (focoJogador1, novoFoco)

    let proximoJogador = if jogadorAtual == jogador1 then jogador2 else jogador1
    
    rodadaJogador novoTPassado2 novoTPresente2 novoTFuturo2 proximoJogador novoFocoJogador1 novoFocoJogador2

jogar :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> IO (Tabuleiro, Tabuleiro, Tabuleiro)
jogar tPassado tPresente tFuturo jogadorAtual foco = do
    putStrLn "\nTabuleiros atuais:"
    imprimirTabuleiros tPassado tPresente tFuturo

    let tabuleiroSelecionado
          | foco == "passado"  = tPassado
          | foco == "presente" = tPresente
          | otherwise  = tFuturo

    if verificarJogadorTabuleiro jogadorAtual tabuleiroSelecionado
        then do
            jogada <- escolherJogada

            -- Se a jogada for "v" (viagem no tempo), definir o destino da viagem
            viagem <- if jogada == "v"
                then definirFoco "src/Interface/viagem.txt" foco
                else return ""  -- Retorna string vazia para os outros casos

            (linhaOrigem, colunaOrigem) <- obterJogada "Coordenadas de Origem: "
            (linhaDestino, colunaDestino) <- obterJogada "Coordenadas de Destino: "

            if movimentoValido (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino)
                then do
                    if (jogada == "m")
                        then do
                            movimentarPeca tabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual foco linhaOrigem colunaOrigem linhaDestino colunaDestino
                    else if (jogada == "p")
                        then do
                            movimentarPeca tabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual foco linhaOrigem colunaOrigem linhaDestino colunaDestino
                    else if (jogada == "v")
                        then do
                            putStrLn viagem
                            movimentarPeca tabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual foco linhaOrigem colunaOrigem linhaDestino colunaDestino
                        else do
                            putStrLn "Opção inválida!"
                            jogar tPassado tPresente tFuturo jogadorAtual foco
                else do
                    putStrLn "Movimento inválido! Você só pode se mover uma casa na horizontal ou na vertical."
                    jogar tPassado tPresente tFuturo jogadorAtual foco
        else do
            putStrLn "Erro: Jogador não encontrado!"
            jogar tPassado tPresente tFuturo jogadorAtual foco
