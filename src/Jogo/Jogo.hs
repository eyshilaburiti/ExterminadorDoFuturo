module Jogo.Jogo where
    
import Jogo.Tabuleiro (Tabuleiro, imprimirTabuleiros, jogador1, jogador2, tabuleiro4x4, inicializarTabuleiro, movimentoValido, verificarJogadorTabuleiro, verificarVitoria)
import Interface.Jogador (obterJogadaOrigem, obterJogadaDestino, definirFoco, escolherJogada)
import Jogo.MovimentarPeca (movimentarPeca)
import Jogo.ViagemTempo(defineViagem, posicaoLivre, viagem)
import Jogo.PlantarSemente (plantarSemente)
import Jogo.RemoveSemente (removeSemente)
import Utils.ImprimirTxt (imprimirTxt)
import System.IO (hFlush, stdout)
import Jogo.Bot(escolherJogadaBot, escolherTempoBot, escolherOrigemBot, escolherDestinoBot, escolherFocoBot)
import Control.Concurrent (threadDelay)

iniciarTabuleiro :: IO () 
iniciarTabuleiro = do
    let tabuleiro1 = inicializarTabuleiro tabuleiro4x4 0 0 jogador1
    let tabuleiro = inicializarTabuleiro tabuleiro1 3 3 jogador2

    let tabuleiroPassado = tabuleiro
    let tabuleiroPresente = tabuleiro
    let tabuleiroFuturo = tabuleiro

    regras <- visualizarRegras ()
    if regras == True then imprimirTxt "src/Interface/regras.txt"
    else putStr ""
    

    bot <- escolheModoDeJogo "src/Interface/modoDeJogo.txt"

    rodadaJogador tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador1 "passado" "futuro" 0 0 bot

rodadaJogador :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> String -> Int -> Int -> Bool -> IO()
rodadaJogador tPassado tPresente tFuturo jogadorAtual focoJogador1 focoJogador2 clonesJogador1 clonesJogador2 bot = do
    let foco = if jogadorAtual == jogador1 then focoJogador1 else focoJogador2
    let clones = if jogadorAtual == jogador1 then clonesJogador1 else clonesJogador2

    putStr ("Seu foco na rodada atual é o " ++ foco)

    putStrLn $ "\nTurno do jogador: " ++ jogadorAtual
    (novoTPassado1, novoTPresente1, novoTFuturo1, novoFoco1, novoClone1) <- jogar tPassado tPresente tFuturo jogadorAtual foco clones bot
    let vitoria1 = verificarVitoria novoTPassado1 novoTPresente1 novoTFuturo1 jogador1 jogador2
    if (fst vitoria1) then do
        finalizarJogo (snd vitoria1)  -- Finaliza o jogo quando a vitória for detectada
        return ()  -- Para garantir que o fluxo da função termine aqui
    else do
        (novoTPassado2, novoTPresente2, novoTFuturo2, novoFoco2, novoClone2) <- jogar novoTPassado1 novoTPresente1 novoTFuturo1 jogadorAtual novoFoco1 novoClone1 bot

        -- Verifica vitória após a segunda jogada
        let vitoria2 = verificarVitoria novoTPassado2 novoTPresente2 novoTFuturo2 jogador1 jogador2
        if (fst vitoria2) 
            then do
                finalizarJogo (snd vitoria2)
                return ()
            else do
                imprimirTabuleiros novoTPassado2 novoTPresente2 novoTFuturo2

                novoFoco <- if ehBot jogadorAtual bot
                    then do
                        focoBot <- escolherFocoBot novoTPassado2 novoTPresente2 novoTFuturo2 jogadorAtual foco
                        putStrLn $ "O foco do bot na próxima rodada será: " ++ focoBot ++ "\n" 
                        return focoBot
                    else definirFoco "src/Interface/foco.txt" novoTPassado2 novoTPresente2 novoTFuturo2 jogadorAtual foco


                let (novoFocoJogador1, novoFocoJogador2) =
                        if jogadorAtual == jogador1
                            then (novoFoco, focoJogador2)
                            else (focoJogador1, novoFoco)
                    
                let (clonesAtualizadosJogador1, clonesAtualizadosJogador2) =
                        if jogadorAtual == jogador1
                            then (novoClone2, clonesJogador2)
                            else (clonesJogador1, novoClone2)

                let proximoJogador = if jogadorAtual == jogador1 then jogador2 else jogador1

                rodadaJogador novoTPassado2 novoTPresente2 novoTFuturo2 proximoJogador novoFocoJogador1 novoFocoJogador2 clonesAtualizadosJogador1 clonesAtualizadosJogador2 bot

jogar :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> Int -> Bool -> IO (Tabuleiro, Tabuleiro, Tabuleiro, String, Int)
jogar tPassado tPresente tFuturo jogadorAtual foco clones bot = do
    putStrLn "\nTabuleiros atuais:"
    imprimirTabuleiros tPassado tPresente tFuturo

    let tabuleiroSelecionado
          | foco == "passado"  = tPassado
          | foco == "presente" = tPresente
          | otherwise  = tFuturo

    if verificarJogadorTabuleiro jogadorAtual tabuleiroSelecionado
        then do
            -- Se o jogador é um bot chama a função para ele escolher sua jogada
            jogada <- if ehBot jogadorAtual bot
                then do 
                    jogadaBot <- escolherJogadaBot
                    threadDelay (2 * 1000000)  -- 2 seconds
                    putStrLn $ "A jogada escolhida pelo bot foi: " ++ jogadaBot ++ "\n"
                    return jogadaBot
                else escolherJogada


            -- Se a jogada for "v" (viagem no tempo), definir o destino da viagem
            novoTempo <- if jogada == "v"
                then if ehBot jogadorAtual bot
                    then do
                        tempoBot <- escolherTempoBot foco
                        threadDelay (2 * 1000000)  -- 2 seconds
                        putStrLn $ "Tempo escolhido pelo bot: " ++ tempoBot
                        return tempoBot
                    else defineViagem "src/Interface/viagem.txt" foco clones
                else return ""  -- Retorna string vazia para outros casos


            if novoTempo == "viagem impossível"
                then jogar tPassado tPresente tFuturo jogadorAtual foco clones bot
                else do
                    -- se um bot for o jogador atual ele escolhe a sua jogada
                    (linhaOrigem, colunaOrigem) <- if ehBot jogadorAtual bot
                        then do
                            threadDelay (2 * 1000000)  -- 2 seconds
                            (linhaOrigemBot, colunaOrigemBot) <- escolherOrigemBot tabuleiroSelecionado jogadorAtual
                            putStrLn $ "Origem escolhida pelo bot: " ++ show (linhaOrigemBot + 1, colunaOrigemBot + 1)
                            return (linhaOrigemBot, colunaOrigemBot)                       
                        else obterJogadaOrigem "Coordenadas de Origem: " jogadorAtual tabuleiroSelecionado

                    case jogada of
                        "v" -> do
                            -- Quando a posição não está livre, refaz a jogada
                            if not (posicaoLivre tPassado tPresente tFuturo novoTempo linhaOrigem colunaOrigem)
                                then do 
                                    putStrLn "Posição ocupada, escolha outra jogada ou viaje para outro tempo."
                                    jogar tPassado tPresente tFuturo jogadorAtual foco clones bot
                                else do
                                    let (novoTPassado, novoTPresente, novoTFuturo, novosClones) = 
                                            viagem tPassado tPresente tFuturo novoTempo foco linhaOrigem colunaOrigem clones jogadorAtual
                                    -- Atualiza o foco para o novo tempo após a viagem e também a quantidade de clones
                                    if ehBot jogadorAtual bot 
                                        then putStrLn $ "O bot viajou para o " ++ novoTempo
                                        else putStrLn ""
                                    return (novoTPassado, novoTPresente, novoTFuturo, novoTempo, novosClones)
                        
                        {-"m" -> do
                            -- se o jogador for um bot ele escolhe sua coluna de destino
                            (linhaDestino, colunaDestino) <- if ehBot jogadorAtual bot
                                then do
                                    (linhaDestinoBot, colunaDestinoBot) <- escolherDestinoBot (linhaOrigem, colunaOrigem)
                                    threadDelay (2 * 1000000)  -- 2 seconds
                                    putStrLn $ "Destino escolhido pelo bot: " ++ show (linhaDestinoBot + 1, colunaDestinoBot + 1)
                                    return (linhaDestinoBot, colunaDestinoBot)                               
                                else obterJogadaDestino "src/Interface/movimento.txt" linhaOrigem colunaOrigem jogadorAtual

                            if movimentoValido tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino) 
                                then do
                                    (novoTPassado, novoTPresente, novoTFuturo) <- 
                                        movimentarPeca tabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual foco linhaOrigem colunaOrigem linhaDestino colunaDestino
                                    return (novoTPassado, novoTPresente, novoTFuturo, foco, clones)
                                else do
                                    putStrLn "Movimento inválido! Você só pode se mover uma casa na horizontal ou na vertical."
                                    jogar tPassado tPresente tFuturo jogadorAtual foco clones bot-}
                        "m" -> do
                            -- se o jogador for um bot ele escolhe sua coluna de destino
                            (linhaDestino, colunaDestino) <- if ehBot jogadorAtual bot
                                then do
                                    (linhaDestinoBot, colunaDestinoBot) <- escolherDestinoBot (linhaOrigem, colunaOrigem)
                                    threadDelay (2 * 1000000)  -- 2 seconds
                                    putStrLn $ "Destino escolhido pelo bot: " ++ show (linhaDestinoBot + 1, colunaDestinoBot + 1)
                                    return (linhaDestinoBot, colunaDestinoBot)                               
                                else obterJogadaDestino "src/Interface/movimento.txt" linhaOrigem colunaOrigem jogadorAtual

                            if movimentoValido tabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino) 
                                then do
                                    (novoTPassado, novoTPresente, novoTFuturo, jogadorMorreu) <- 
                                        movimentarPeca tabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual foco linhaOrigem colunaOrigem linhaDestino colunaDestino
                                    if jogadorMorreu
                                        then do
                                            putStrLn "Jogador morreu ao entrar na planta!"
                                            return (novoTPassado, novoTPresente, novoTFuturo, jogadorAtual, clones)
                                        else
                                            return (novoTPassado, novoTPresente, novoTFuturo, foco, clones)
                                else do
                                    putStrLn "Movimento inválido! Você só pode se mover uma casa na horizontal ou na vertical."
                                    jogar tPassado tPresente tFuturo jogadorAtual foco clones bot

                        "p" -> do
                            -- se o jogador for um bot ele escolhe sua coluna de destino
                            (linhaDestino, colunaDestino) <- if ehBot jogadorAtual bot
                                then do
                                    (linhaDestinoBot, colunaDestinoBot) <- escolherDestinoBot (linhaOrigem, colunaOrigem)
                                    threadDelay (2 * 1000000)  -- 2 seconds
                                    putStrLn $ "Destino escolhido pelo bot: " ++ show (linhaDestinoBot + 1, colunaDestinoBot + 1)
                                    return (linhaDestinoBot, colunaDestinoBot)                               
                                else obterJogadaDestino "src/Interface/movimento.txt" linhaOrigem colunaOrigem jogadorAtual

                            (novoTPassado, novoTPresente, novoTFuturo) <- 
                                plantarSemente tabuleiroSelecionado tPassado tPresente tFuturo foco linhaDestino colunaDestino
                            return (novoTPassado, novoTPresente, novoTFuturo, foco, clones)

                        _ -> do
                            putStrLn "Opção inválida!"
                            jogar tPassado tPresente tFuturo jogadorAtual foco clones bot

        else do
            putStrLn "Erro: Jogador não encontrado!"
            jogar tPassado tPresente tFuturo jogadorAtual foco clones bot

escolheModoDeJogo :: String -> IO Bool
escolheModoDeJogo mensagem = do
    imprimirTxt mensagem
    putStrLn "Escolha uma opção digitando o modo correspondente (s para sozinho, d para dois jogadores): "
    hFlush stdout
    modo <- getLine
    case modo of
        "s" -> return True
        "d" -> return False
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            escolheModoDeJogo mensagem

visualizarRegras :: () -> IO Bool
visualizarRegras mensagem = do 
    putStr "Deseja ver as regras do jogo?(s/n) "
    hFlush stdout
    resposta <- getLine
    case resposta of 
        "s" -> return True
        "n" -> return False 
        _   -> do 
            putStrLn "Opção inválida. Tente novamente."
            visualizarRegras ()

-- Exibe mensagem de fim de jogo
finalizarJogo :: String -> IO ()
finalizarJogo jogadorVencedor = do
    imprimirTxt "src/Interface/fimDeJogo.txt"
    putStrLn $ "O jogador " ++ jogadorVencedor ++ " venceu o jogo!"

ehBot :: String -> Bool -> Bool
ehBot jogadorAtual bot = jogadorAtual == jogador2 && bot
