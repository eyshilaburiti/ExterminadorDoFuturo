module Utils.Jogo (iniciarTabuleiro) where
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Utils.Tabuleiro

iniciarTabuleiro :: IO ()
iniciarTabuleiro = do
    let tabuleiro1 = inicializarTabuleiro tabuleiro4x4 0 0 jogador1
    let tabuleiro = inicializarTabuleiro tabuleiro1 3 3 jogador2

    let tabuleiroPassado = tabuleiro
    let tabuleiroPresente = tabuleiro
    let tabuleiroFuturo = tabuleiro

    jogar tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador1

-- Loop do jogo, alternando entre os jogadores
jogar :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> IO ()
jogar tPassado tPresente tFuturo jogadorAtual = do
    putStrLn "\nTabuleiros atuais:"
    imprimirTabuleiros tPassado tPresente tFuturo

    putStrLn $ "\nVez do jogador: " ++ jogadorAtual
    foco <- definirFoco
    (linha, coluna, jogada) <- escolherJogada

    -- usar guardas
    let tabuleiroSelecionado
          | foco == "passado"  = tPassado
          | foco == "presente" = tPresente
          | foco == "futuro"   = tFuturo
          | otherwise          = tPresente -- fallback para evitar erros

    let posicaoAtual = acharJogador jogadorAtual tabuleiroSelecionado

    case posicaoAtual of
        Just (linhaAntiga, colunaAntiga) ->
            if movimentoValido (linhaAntiga, colunaAntiga) (linha, coluna) then do

                if (jogada == "a") then do 
                    let proximoJogador = if jogadorAtual == jogador1 then jogador2 else jogador1
                    if(foco == "passado") then do 
                        let novoTabuleiro = plantarSementeNoTabuleiro tabuleiroSelecionado linha coluna semente
                        let novoTabuleiroPresente = plantarSementeNoTabuleiro tPresente linha coluna arbusto
                        let novoTabuleiroFuturo = plantarSementeNoTabuleiro tFuturo linha coluna arvore
                        
                        jogar novoTabuleiro novoTabuleiroPresente novoTabuleiroFuturo proximoJogador

                    else if(foco == "presente") then do 
                        let novoTabuleiro = plantarSementeNoTabuleiro tabuleiroSelecionado linha coluna semente
                        let novoTabuleiroFuturo = plantarSementeNoTabuleiro tFuturo linha coluna arbusto

                        jogar tPassado novoTabuleiro novoTabuleiroFuturo proximoJogador

                    else if(foco == "futuro") then do 
                        let novoTabuleiro = plantarSementeNoTabuleiro tabuleiroSelecionado linha coluna semente

                        jogar tPassado tPresente novoTabuleiro proximoJogador

                    else jogar tPassado tPresente tFuturo proximoJogador
                    
                    
                else do 
                    let novoTabuleiro = atualizarTabuleiro tabuleiroSelecionado linhaAntiga colunaAntiga linha coluna jogadorAtual
                    
                    let (novoTPassado, novoTPresente, novoTFuturo)
                          | foco == "passado" = (novoTabuleiro, tPresente, tFuturo)
                          | foco == "presente" = (tPassado, novoTabuleiro, tFuturo)
                          | foco == "futuro" = (tPassado, tPresente, novoTabuleiro)
                          | otherwise        = (tPassado, tPresente, tFuturo)

                    let proximoJogador = if jogadorAtual == jogador1 then jogador2 else jogador1
                    jogar novoTPassado novoTPresente novoTFuturo proximoJogador

            else do
                putStrLn "Movimento inválido! Você só pode se mover uma casa na horizontal ou na vertical."
                jogar tPassado tPresente tFuturo jogadorAtual
        Nothing -> do
            putStrLn "Erro: Jogador não encontrado!"
            jogar tPassado tPresente tFuturo jogadorAtual

    case posicaoAtual of
        Just (linhaAntiga, colunaAntiga) ->
            if movimentoValido (linhaAntiga, colunaAntiga) (linha, coluna) then do
                let novoTabuleiro = atualizarTabuleiro tabuleiroSelecionado linhaAntiga colunaAntiga linha coluna jogadorAtual
                
                let (novoTPassado, novoTPresente, novoTFuturo)
                      | foco == "passado"  = (novoTabuleiro, tPresente, tFuturo)
                      | foco == "presente" = (tPassado, novoTabuleiro, tFuturo)
                      | foco == "futuro"   = (tPassado, tPresente, novoTabuleiro)
                      | otherwise          = (tPassado, tPresente, tFuturo)

                let proximoJogador = if jogadorAtual == jogador1 then jogador2 else jogador1
                jogar novoTPassado novoTPresente novoTFuturo proximoJogador
            else do
                putStrLn "Movimento inválido! Você só pode se mover uma casa na horizontal ou na vertical."
                jogar tPassado tPresente tFuturo jogadorAtual
        Nothing -> do
            putStrLn "Erro: Jogador não encontrado!"
            jogar tPassado tPresente tFuturo jogadorAtual



--Função onde o jogador escolhe tipo de jogada
escolherJogada :: IO (Int, Int, String)
escolherJogada = do 
    putStr "Escolha qual será sua próxima rodada: (Movimentação (m), Plantar Árvore (a)) "
    hFlush stdout
    escolha <- getLine
    if escolha == "m" then do
        (linha, coluna) <- obterJogada
        return (linha, coluna, escolha)
    else if escolha == "a" then do
        (linha, coluna) <- obterLocalSemente
        return (linha, coluna, escolha)
    else do 
        putStrLn "Entrada inválida!"
        escolherJogada

-- Solicitar jogada do jogador
obterJogada :: IO (Int, Int)
obterJogada = do
    linha <- obterPosicao "Digite a linha (1 a 4): "
    coluna <- obterPosicao "Digite a coluna (1 a 4): "
    return (linha, coluna)

-- Função para plantar semente
obterLocalSemente :: IO (Int, Int)
obterLocalSemente = do
    linha <- obterPosicao "Digite a linha onde deseja plantar a semente: (1 a 4)"
    coluna <- obterPosicao "Digite a coluna onde deseja plantar a semente: (1 a 4)"
    return (linha, coluna)

-- Função para obter um número válido dentro de um intervalo específico
obterPosicao :: String -> IO Int
obterPosicao mensagem = do
    putStr mensagem
    hFlush stdout
    posicao <- getLine
    case readMaybe posicao :: Maybe Int of
        Just n ->
            if n > 0 && n <= 4
                then return (n - 1)
            else do
                putStrLn "Posição inválida! Tente novamente."
                obterPosicao mensagem
        Nothing -> do
            putStrLn "Entrada inválida!"
            obterPosicao mensagem

-- Função que define o foco do jogador
definirFoco :: IO String 
definirFoco = do
    putStr "Escolha o foco para a próxima rodada (passado, presente, futuro): "
    hFlush stdout
    foco <- getLine

    if foco == "passado"
        then return foco
    else if foco == "presente"
        then return foco
    else if foco == "futuro"
        then return foco
    else do
        putStrLn "Opção Inválida"
        definirFoco  -- Chama a função novamente até que o jogador insira um valor válido.

