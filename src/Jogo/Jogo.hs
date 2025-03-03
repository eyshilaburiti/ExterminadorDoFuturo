module Jogo.Jogo (iniciarJogo) where
    
import Jogo.Tabuleiro (Tabuleiro, imprimirTabuleiros, jogador1, jogador2, tabuleiro4x4, inicializarTabuleiro, movimentoValido, verificarJogadorTabuleiro, verificarVitoria, posicaoOcupada, selecionarTabuleiro)
import Interface.Jogador (obterJogadaOrigem, obterJogadaDestino, definirFoco, escolherJogada, escolherOpcaoMenu, exibirOpcaoMenu, jogadorNoFoco)
import Jogo.MovimentarPeca (movimentarPeca)
import Jogo.ViagemTempo(defineViagem, posicaoLivre, viagem)
import Jogo.ControladorPlantas (plantarSemente)
import Utils.ImprimirTxt (imprimirTxt)
import System.IO (hFlush, stdout)
import Utils.Bot (escolherJogadaBot, escolherTempoBot, escolherOrigemBot, escolherDestinoBot, escolherFocoBot)
import Control.Concurrent (threadDelay)
import Utils.Ranking (atualizarRanking, mostrarRanking)
import Data.Char (toLower)
import System.Exit (exitSuccess)

iniciarJogo :: IO ()
iniciarJogo = do
    imprimirTxt "src/Interface/exterminadorDoFuturo.txt"
    iniciarTabuleiro

registrarJogadores :: IO ((String, String), (String, String))
registrarJogadores = do
    putStrLn "\nRegistro dos jogadores!"

    putStr "Jogador 1, digite seu nome: "
    hFlush stdout
    nome1 <- getLine
    let nome1Min = removerEspacos nome1  -- Remove todos os espaços e converte para minúsculo
    let jog1 = jogador1
    putStrLn ("Seu personagem será a " ++ jogador1)

    putStr "Jogador 2, digite seu nome: "
    hFlush stdout
    nome2 <- getLine
    let nome2Min = removerEspacos nome2
    let jog2 = jogador2
    putStrLn ("Jogador 2 ficará com o " ++ jogador2)

    return ((nome1Min, jog1), (nome2Min, jog2))

removerEspacos :: String -> String
removerEspacos = filter (/= ' ') . map toLower

registrarJogadorUnico :: IO (String, String)
registrarJogadorUnico = do
    putStrLn "\nRegistro do jogador!"
    putStr "Digite seu nome: "
    hFlush stdout
    nome <- getLine
    let nomeMin = removerEspacos nome
    let jogador = jogador1 
    putStrLn ("Seu personagem será a " ++ jogador1)

    return (nomeMin, jogador)

iniciarTabuleiro :: IO () 
iniciarTabuleiro = do
    let tabuleiro1 = inicializarTabuleiro tabuleiro4x4 0 0 jogador1
    let tabuleiro = inicializarTabuleiro tabuleiro1 3 3 jogador2

    let tabuleiroPassado = tabuleiro
    let tabuleiroPresente = tabuleiro
    let tabuleiroFuturo = tabuleiro

    opcaoMenu <- escolherOpcaoMenu
    exibirOpcaoMenu opcaoMenu

    if opcaoMenu == "j" then do
        bot <- escolheModoDeJogo
        if not bot then do
            ((nome1, jog1), (nome2, jog2)) <- registrarJogadores
            --let (tabuleiroPassado, tabuleiroPresente, tabuleiroFuturo) = inicioTab jog1 jog2
            rodadaJogador tabuleiroPassado tabuleiroPresente tabuleiroFuturo jog1 nome1 nome1 jog1 nome2 jog2 "passado" "futuro" 0 0 bot
        else do
            (nome1, jog1) <- registrarJogadorUnico
            let nome2 = "bot"
            let jog2 = jogador2 
            --let (tabuleiroPassado, tabuleiroPresente, tabuleiroFuturo) = inicioTab jog1 jog2
            rodadaJogador tabuleiroPassado tabuleiroPresente tabuleiroFuturo jog1 nome1 nome1 jog1 nome2 jog2 "passado" "futuro" 0 0 bot
    else if opcaoMenu == "s" then exitSuccess
    else do 
        iniciarTabuleiro


rodadaJogador :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String-> String -> String -> String -> String -> String -> String -> Int -> Int -> Bool -> IO()
rodadaJogador tPassado tPresente tFuturo jogadorAtual nomeAtual nome1 jog1 nome2 jog2 focoJogador1 focoJogador2 clonesJogador1 clonesJogador2 bot = do

    let foco = if jogadorAtual == jog1 then focoJogador1 else focoJogador2
    let clones = if jogadorAtual == jog1 then clonesJogador1 else clonesJogador2
    
    (novoTPassado1, novoTPresente1, novoTFuturo1, novoFoco1, novoClone1) <- jogar tPassado tPresente tFuturo jogadorAtual nomeAtual foco clones bot

    -- Verifica vitória após a primeira jogada
    let (venceu1, emojiVencedor1, nomeVencedor1) = verificarVitoria novoTPassado1 novoTPresente1 novoTFuturo1 jog1 nome1 jog2 nome2
    if venceu1 then do
        let nomePerdedor = if nomeVencedor1 == nome1 then nome2 else nome1
        finalizarJogo emojiVencedor1 nomeVencedor1 nomePerdedor
        return ()  
    else do
        (novoTPassado2, novoTPresente2, novoTFuturo2, novoFoco2, novoClone2) <- jogar novoTPassado1 novoTPresente1 novoTFuturo1 jogadorAtual nomeAtual novoFoco1 novoClone1 bot

        -- Verifica vitória após a segunda jogada
        let (venceu2, emojiVencedor2, nomeVencedor2) = verificarVitoria novoTPassado2 novoTPresente2 novoTFuturo2 jog1 nome1 jog2 nome2
        if venceu2 then do
            let nomePerdedor = if nomeVencedor2 == nome1 then nome2 else nome1
            finalizarJogo emojiVencedor2 nomeVencedor2 nomePerdedor
            return ()
        else do
            imprimirTxt "src/Interface/delimitadorInicial.txt"
            imprimirTabuleiros novoTPassado2 novoTPresente2 novoTFuturo2
            exibeFoco novoFoco2
            imprimirTxt "src/Interface/delimitadorFinal.txt"

            novoFoco <- if ehBot jogadorAtual bot
                then do
                    focoBot <- escolherFocoBot novoTPassado2 novoTPresente2 novoTFuturo2 jogadorAtual foco
                    putStrLn $ "O foco do bot na próxima rodada será: " ++ focoBot ++ "\n" 
                    return focoBot
                else definirFoco "src/Interface/foco.txt" novoTPassado2 novoTPresente2 novoTFuturo2 jogadorAtual foco

            let (novoFocoJogador1, novoFocoJogador2) =
                    if jogadorAtual == jog1
                        then (novoFoco, focoJogador2)
                        else (focoJogador1, novoFoco)
                
            let (clonesAtualizadosJogador1, clonesAtualizadosJogador2) =
                    if jogadorAtual == jog1
                        then (novoClone2, clonesJogador2)
                        else (clonesJogador1, novoClone2)
            
            let proximoJogador = if jogadorAtual == jog1 then jog2 else jog1
            let proxNome = if proximoJogador == jog1 then nome1 else nome2

            rodadaJogador novoTPassado2 novoTPresente2 novoTFuturo2 proximoJogador proxNome nome1 jog1 nome2 jog2 novoFocoJogador1 novoFocoJogador2 clonesAtualizadosJogador1 clonesAtualizadosJogador2 bot


jogar :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> String -> String -> Int -> Bool -> IO (Tabuleiro, Tabuleiro, Tabuleiro, String, Int)
jogar tPassado tPresente tFuturo jogadorAtual nomeAtual foco clones bot = do
    imprimirTxt "src/Interface/delimitadorInicial.txt"
    putStr ("- Foco atual: " ++ foco)
    putStrLn $ "\nTurno do jogador: " ++ jogadorAtual ++ " " ++ "(" ++ nomeAtual ++ ")"-- mostra o emoji 
    putStrLn ""
    imprimirTabuleiros tPassado tPresente tFuturo
    exibeFoco foco
    imprimirTxt "src/Interface/delimitadorFinal.txt"

    let tabuleiroSelecionado
          | foco == "passado"  = tPassado
          | foco == "presente" = tPresente
          | otherwise  = tFuturo

    -- Verifica se o jogador está no foco atual, ajusta o foco e o tabuleiro se necessário
    (novoFoco, novoTabuleiroSelecionado) <- if not (jogadorNoFoco tabuleiroSelecionado foco jogadorAtual)
        then if ehBot jogadorAtual bot
            then do
                focoBot <- escolherFocoBot tPassado tPresente tFuturo jogadorAtual foco
                putStrLn $ "\x2757 O jogador não foi encontrado no tabuleiro, o bot escolheu o novo foco: " ++ focoBot ++ "\n"
                return (focoBot, selecionarTabuleiro focoBot tPassado tPresente tFuturo)

            else do
                putStrLn "\x2757 O jogador não foi encontrado no tabuleiro, escolha o foco novamente:"
                focoJogador <- definirFoco "src/Interface/foco.txt" tPassado tPresente tFuturo jogadorAtual foco
                return (focoJogador, selecionarTabuleiro focoJogador tPassado tPresente tFuturo)
        else return (foco, tabuleiroSelecionado)



    if verificarJogadorTabuleiro jogadorAtual novoTabuleiroSelecionado
        then do
            (linhaOrigem, colunaOrigem) <- if ehBot jogadorAtual bot
                then do
                    threadDelay (2 * 1000000)  -- 2 segundos
                    (linhaOrigemBot, colunaOrigemBot) <- escolherOrigemBot novoTabuleiroSelecionado jogadorAtual
                    putStrLn $ "Origem escolhida pelo bot: " ++ show (linhaOrigemBot + 1, colunaOrigemBot + 1)
                    return (linhaOrigemBot, colunaOrigemBot)
                else obterJogadaOrigem "Coordenadas de Origem: " jogadorAtual novoTabuleiroSelecionado


            -- Se o jogador é um bot chama a função para ele escolher sua jogada
            jogada <- if ehBot jogadorAtual bot
                then do 
                    jogadaBot <- escolherJogadaBot novoTabuleiroSelecionado (linhaOrigem, colunaOrigem) jogadorAtual
                    threadDelay (2 * 1000000)  -- 2 seconds
                    putStrLn $ "A jogada escolhida pelo bot foi: " ++ jogadaBot ++ "\n"
                    return jogadaBot
                else escolherJogada



            -- Se a jogada for "v" (viagem no tempo), definir o destino da viagem
            novoTempo <- if jogada == "v"
                then if ehBot jogadorAtual bot
                    then do
                        tempoBot <- escolherTempoBot novoFoco
                        threadDelay (2 * 1000000)  -- 2 seconds
                        putStrLn $ "Tempo escolhido pelo bot: " ++ tempoBot
                        return tempoBot
                    else defineViagem "src/Interface/viagem.txt" novoFoco clones
                else return ""  -- Retorna string vazia para outros casos


            case jogada of
                "v" -> do
                    -- Verifica se a viagem é possível
                    if novoTempo == "viagem impossível"
                        then jogar tPassado tPresente tFuturo jogadorAtual nomeAtual novoFoco clones bot
                    else do
                        -- Verifica se a posição de destino está livre
                        if not (posicaoLivre tPassado tPresente tFuturo novoTempo linhaOrigem colunaOrigem)
                            then do 
                                putStrLn "Posição ocupada, escolha outra jogada ou viaje para outro tempo."
                                jogar tPassado tPresente tFuturo jogadorAtual nomeAtual novoFoco clones bot
                            else do
                                -- Realiza a viagem no tempo
                                let (novoTPassado, novoTPresente, novoTFuturo, novosClones) =
                                        viagem tPassado tPresente tFuturo novoTempo novoFoco linhaOrigem colunaOrigem clones jogadorAtual

                                if ehBot jogadorAtual bot
                                    then putStrLn $ "O bot viajou para o " ++ novoTempo
                                    else putStrLn ""

                                -- Retorna o novo estado do jogo
                                return (novoTPassado, novoTPresente, novoTFuturo, novoTempo, novosClones)

                "m" -> do
                    (linhaDestino, colunaDestino) <- if ehBot jogadorAtual bot
                        then do
                            (linhaDestinoBot, colunaDestinoBot) <- escolherDestinoBot novoTabuleiroSelecionado (linhaOrigem, colunaOrigem) jogadorAtual
                            threadDelay (2 * 1000000)
                            putStrLn $ "Destino escolhido pelo bot: " ++ show (linhaDestinoBot + 1, colunaDestinoBot + 1)
                            return (linhaDestinoBot, colunaDestinoBot)
                        else obterJogadaDestino "src/Interface/movimento.txt" linhaOrigem colunaOrigem jogadorAtual

                    if movimentoValido novoTabuleiroSelecionado (linhaOrigem, colunaOrigem) (linhaDestino, colunaDestino)
                        then do
                            (novoTPassado, novoTPresente, novoTFuturo, jogadorMorreu) <- 
                                movimentarPeca novoTabuleiroSelecionado tPassado tPresente tFuturo jogadorAtual novoFoco linhaOrigem colunaOrigem linhaDestino colunaDestino
                            if jogadorMorreu
                                then do
                                    putStrLn "Jogador morreu ao entrar na planta! \x1F480"
                                    return (novoTPassado, novoTPresente, novoTFuturo, jogadorAtual, clones)
                                else
                                    return (novoTPassado, novoTPresente, novoTFuturo, novoFoco, clones)
                        else do
                            putStrLn "\x274C Movimento inválido! Você só pode se mover uma casa na horizontal ou vertical."
                            jogar tPassado tPresente tFuturo jogadorAtual nomeAtual novoFoco clones bot

                "p" -> do
                    (linhaDestino, colunaDestino) <- if ehBot jogadorAtual bot
                        then do
                            (linhaDestinoBot, colunaDestinoBot) <- escolherDestinoBot novoTabuleiroSelecionado (linhaOrigem, colunaOrigem) jogadorAtual
                            threadDelay (2 * 1000000)
                            putStrLn $ "Destino escolhido pelo bot: " ++ show (linhaDestinoBot + 1, colunaDestinoBot + 1)
                            return (linhaDestinoBot, colunaDestinoBot)
                        else obterJogadaDestino "src/Interface/plantar.txt" linhaOrigem colunaOrigem jogadorAtual

                    if not (posicaoOcupada novoTabuleiroSelecionado linhaDestino colunaDestino)
                        then do
                            (novoTPassado, novoTPresente, novoTFuturo) <- 
                                plantarSemente novoTabuleiroSelecionado tPassado tPresente tFuturo novoFoco linhaDestino colunaDestino
                            return (novoTPassado, novoTPresente, novoTFuturo, novoFoco, clones)
                        else do
                            putStrLn "\x274C Local inválido! local já está ocupado"
                            jogar tPassado tPresente tFuturo jogadorAtual nomeAtual novoFoco clones bot

                _ -> do
                    putStrLn "\x274C Opção inválida!"
                    jogar tPassado tPresente tFuturo jogadorAtual nomeAtual novoFoco clones bot

        else do
            putStrLn "\x2757 Erro: Jogador não encontrado!"
            jogar tPassado tPresente tFuturo jogadorAtual nomeAtual novoFoco clones bot

escolheModoDeJogo :: IO Bool
escolheModoDeJogo = do
    imprimirTxt  "src/Interface/escolherModoDeJogo.txt"
    hFlush stdout
    modo <- getLine
    let modoMinuscula = unwords . words $ map toLower modo

    case modoMinuscula of
        "s" -> return True
        "d" -> return False
        _   -> do
            putStrLn "\x2757 Opção inválida. Tente novamente."
            escolheModoDeJogo

-- Exibe mensagem de fim de jogo
finalizarJogo :: String -> String -> String -> IO ()
finalizarJogo jogadorVencedor nomeVencedor nomePerdedor = do
    imprimirTxt "src/Interface/fimDeJogo.txt"
    putStrLn $ "O jogador " ++ nomeVencedor ++ " (" ++ jogadorVencedor ++ ") venceu a rodada! \x1F3C6"
    atualizarRanking nomeVencedor nomePerdedor 
    mostrarRanking
    iniciarTabuleiro  -- Reinicia o jogo

ehBot :: String -> Bool -> Bool
ehBot jogadorAtual bot = jogadorAtual == jogador2 && bot

exibeFoco :: String -> IO ()
exibeFoco foco
    | foco == "passado" = imprimirTxt "src/Interface/passado.txt"
    | foco == "presente" = imprimirTxt "src/Interface/presente.txt"
    | foco == "futuro" = imprimirTxt "src/Interface/futuro.txt"
    | otherwise = putStr ""