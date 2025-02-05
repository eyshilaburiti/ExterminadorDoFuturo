module Utils.Jogo (jogar) where

-- Solicitar jogada do jogador
obterJogada :: IO (Int, Int)
obterJogada = do
    putStrLn "Digite a linha (1 a 4):"
    linha <- readLn
    putStrLn "Digite a coluna (1 a 4):"
    coluna <- readLn
    if linha > 0 && linha <= 4 && coluna > 0 && coluna <= 4
        then return (linha -1 , coluna -1)
        else do
            putStrLn "Posição inválida! Tente novamente."
            obterJogada

-- Loop do jogo, alternando entre os jogadores
jogar :: Tabuleiro -> Tabuleiro -> Tabuleiro -> String -> IO ()
jogar tPassado tPresente tFuturo jogadorAtual = do
    putStrLn "\nTabuleiros atuais:"
    imprimirTabuleiros tPassado tPresente tFuturo

    putStrLn $ "\nVez do jogador: " ++ jogadorAtual
    foco <- definirFoco
    (linha, coluna) <- obterJogada

    let tabuleiroSelecionado = case foco of
            "passado" -> tPassado
            "presente" -> tPresente
            "futuro" -> tFuturo
            _ -> tPresente -- fallback para evitar erros

    -- Verifica se a posição já está ocupada
    if (tabuleiroSelecionado !! linha !! coluna) /= "\x25A1"
        then do
            putStrLn "Posição já ocupada! Escolha outra."
            jogar tPassado tPresente tFuturo jogadorAtual
        else do
            let novoTabuleiro = atualizarTabuleiro tabuleiroSelecionado linha coluna jogadorAtual

            let (novoTPassado, novoTPresente, novoTFuturo) = case foco of
                    "passado"  -> (novoTabuleiro, tPresente, tFuturo)
                    "presente" -> (tPassado, novoTabuleiro, tFuturo)
                    "futuro"   -> (tPassado, tPresente, novoTabuleiro)
                    _          -> (tPassado, tPresente, tFuturo)

            let proximoJogador = if jogadorAtual == jogador1 then jogador2 else jogador1
            jogar novoTPassado novoTPresente novoTFuturo proximoJogador


-- Função que define o foco do jogador
definirFoco :: IO String 
definirFoco = do
    putStrLn "Escolha o foco para a próxima rodada (passado, presente, futuro): "
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
