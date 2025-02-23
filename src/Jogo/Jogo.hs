module Jogo.Jogo where
    
import Jogo.Tabuleiro (Tabuleiro, imprimirTabuleiros, jogador1, jogador2, tabuleiro4x4, inicializarTabuleiro, movimentoValido, verificarJogadorTabuleiro, verificarVitoria)
import Interface.Jogador (obterJogada, definirFoco, escolherJogada)
import Jogo.MovimentarPeca (movimentarPeca)
import Jogo.ViagemTempo(defineViagem, posicaoLivre, viagem)
import Jogo.PlantarSemente (plantarSemente)
import Jogo.RemoveSemente (removeSemente)
import Utils.ImprimirTxt (imprimirTxt)

iniciarTabuleiro :: IO () 
iniciarTabuleiro = do
    let tabuleiro1 = inicializarTabuleiro tabuleiro4x4 0 0 jogador1
    let tabuleiro = inicializarTabuleiro tabuleiro1 3 3 jogador2

    let tabuleiroPassado = tabuleiro
    let tabuleiroPresente = tabuleiro
    let tabuleiroFuturo = tabuleiro

    rodadaJogador tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador1 "passado" "futuro" 0 0

rodadaJogador :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> String -> Int -> Int -> IO()
rodadaJogador tPassado tPresente tFuturo jogadorAtual focoJogador1 focoJogador2 clonesJogador1 clonesJogador2 = do
    let foco = if jogadorAtual == jogador1 then focoJogador1 else focoJogador2
    let clones = if jogadorAtual == jogador1 then clonesJogador1 else clonesJogador2

    putStr ("Seu foco na rodada atual é o " ++ foco)

    putStrLn $ "\nTurno do jogador: " ++ jogadorAtual
    (novoTPassado1, novoTPresente1, novoTFuturo1, novoFoco1, novoClone1) <- jogar tPassado tPresente tFuturo jogadorAtual foco clones
    let vitoria1 = verificarVitoria novoTPassado1 novoTPresente1 novoTFuturo1 jogador1 jogador2
    if (fst vitoria1) then do
        finalizarJogo (snd vitoria1)  -- Finaliza o jogo quando a vitória for detectada
        return ()  -- Para garantir que o fluxo da função termine aqui
    else do
        (novoTPassado2, novoTPresente2, novoTFuturo2, novoFoco2, novoClone2) <- jogar novoTPassado1 novoTPresente1 novoTFuturo1 jogadorAtual novoFoco1 novoClone1

        -- Verifica vitória após a segunda jogada
        let vitoria2 = verificarVitoria novoTPassado2 novoTPresente2 novoTFuturo2 jogador1 jogador2
        if (fst vitoria2) 
            then do
                finalizarJogo (snd vitoria2)
                return ()
            else do
                imprimirTabuleiros novoTPassado2 novoTPresente2 novoTFuturo2

                novoFoco <- definirFoco "src/Interface/foco.txt" foco

                let (novoFocoJogador1, novoFocoJogador2) =
                        if jogadorAtual == jogador1
                            then (novoFoco, focoJogador2)
                            else (focoJogador1, novoFoco)
                    
                let (clonesAtualizadosJogador1, clonesAtualizadosJogador2) =
                        if jogadorAtual == jogador1
                            then (novoClone2, clonesJogador2)
                            else (clonesJogador1, novoClone2)

                let proximoJogador = if jogadorAtual == jogador1 then jogador2 else jogador1

                rodadaJogador novoTPassado2 novoTPresente2 novoTFuturo2 proximoJogador novoFocoJogador1 novoFocoJogador2 clonesAtualizadosJogador1 clonesAtualizadosJogador2

jogar :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> Int-> IO (Tabuleiro, Tabuleiro, Tabuleiro, String, Int)
jogar tPassado tPresente tFuturo jogadorAtual foco clones = do
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
            novoTempo <- if jogada == "v"
                then defineViagem "src/Interface/viagem.txt" foco clones
                else return ""  -- Retorna string vazia para os outros casos

            if novoTempo == "viagem impossível"
                then jogar tPassado tPresente tFuturo jogadorAtual foco clones
                else do
                    (linhaOrigem, colunaOrigem) <- obterJogada "Coordenadas de Origem: "
                    case jogada of
                        "v" -> do
                            -- Quando a posição não está livre, refaz a jogada
                            if not (posicaoLivre tPassado tPresente tFuturo novoTempo linhaOrigem colunaOrigem)
                                then do 
                                    putStrLn "Posição ocupada, escolha outra jogada ou viaje para outro tempo."
                                    jogar tPassado tPresente tFuturo jogadorAtual foco clones
                                else do
                                    let (novoTPassado, novoTPresente, novoTFuturo, novosClones) = 
                                            viagem tPassado tPresente tFuturo novoTempo foco linhaOrigem colunaOrigem clones jogadorAtual
                                    -- Atualiza o foco para o novo tempo após a viagem e também a quantidade de clones
                                    return (novoTPassado, novoTPresente, novoTFuturo, novoTempo, novosClones)
                        
                        "m" -> do
                            (linhaDestino, colunaDestino) <- obterJogada "Coordenadas de Destino: "
                            if movimentoValido tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino) 
                                then do
                                    (novoTPassado, novoTPresente, novoTFuturo) <- 
                                        movimentarPeca tabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual foco linhaOrigem colunaOrigem linhaDestino colunaDestino
                                    return (novoTPassado, novoTPresente, novoTFuturo, foco, clones)
                                else do
                                    putStrLn "Movimento inválido! Você só pode se mover uma casa na horizontal ou na vertical."
                                    jogar tPassado tPresente tFuturo jogadorAtual foco clones

                        "p" -> do
                            (linhaDestino, colunaDestino) <- obterJogada "Coordenadas de Destino: "
                            (novoTPassado, novoTPresente, novoTFuturo) <- 
                                plantarSemente tabuleiroSelecionado tPassado tPresente tFuturo foco linhaDestino colunaDestino
                            return (novoTPassado, novoTPresente, novoTFuturo, foco, clones)
                        
                        "r" -> do
                            (linhaDestino, colunaDestino) <- obterJogada "Coordenadas de Destino: "
                            (novoTPassado, novoTPresente, novoTFuturo) <- 
                                removeSemente tabuleiroSelecionado tPassado tPresente tFuturo foco linhaDestino colunaDestino jogadorAtual
                                
                            return (novoTPassado, novoTPresente, novoTFuturo, foco, clones)

                        _ -> do
                            putStrLn "Opção inválida!"
                            jogar tPassado tPresente tFuturo jogadorAtual foco clones

        else do
            putStrLn "Erro: Jogador não encontrado!"
            jogar tPassado tPresente tFuturo jogadorAtual foco clones

-- Exibe mensagem de fim de jogo
finalizarJogo :: String -> IO ()
finalizarJogo jogadorVencedor = do
    imprimirTxt "src/Interface/fimDeJogo.txt"
    putStrLn $ "O jogador " ++ jogadorVencedor ++ " venceu o jogo!"