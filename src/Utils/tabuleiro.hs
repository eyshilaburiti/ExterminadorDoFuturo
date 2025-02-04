import Data.List (intercalate)

-- Criando o tabuleiro 
type Tabuleiro = [[String]]

tabuleiro4x4 :: Tabuleiro
tabuleiro4x4 = replicate 4 (replicate 4 "\x25A1") -- repetindo o símbolo do quadrado usado

-- Definindo os emojis dos jogadores
jogador1 :: String
jogador1 = "\x265F"

jogador2 :: String
jogador2 = "\x2659"

-- Função para formatar o tabuleiro como uma String 
formatarTabuleiro :: Tabuleiro -> [String]
formatarTabuleiro tabuleiro = 
    let tamanho = length tabuleiro
    -- definindo as bordas
        bordaSuperior = replicate (tamanho * 2 + 1) '_'  
        bordaInferior = replicate (tamanho * 2 + 1) '¯' 
        bordaLateral = map (\linha -> "|" ++ unwords linha ++ "|") tabuleiro
    in bordaSuperior : bordaLateral ++ [bordaInferior]-- juntando as bordas

-- Função para imprimir os 3 tabuleiros
imprimirTabuleiros :: Tabuleiro -> Tabuleiro -> Tabuleiro -> IO ()
imprimirTabuleiros t1 t2 t3 = do
    -- formatando os 3 tabuleiros para a impressão
    let tabuleiro1 = formatarTabuleiro t1
        tabuleiro2 = formatarTabuleiro t2
        tabuleiro3 = formatarTabuleiro t3

    --organizando os tabuleiros para a impressao
    mapM_ (\(linha1, linha2, linha3) -> putStrLn $ linha1 ++ "   " ++ linha2 ++ "   " ++ linha3)
          (zip3 tabuleiro1 tabuleiro2 tabuleiro3)

-- Atualiza o tabuleiro com as jogadas
atualizarTabuleiro :: Tabuleiro -> Int -> Int -> String -> Tabuleiro
atualizarTabuleiro tabuleiro linha coluna jogador = 
    take linha tabuleiro ++ 
    [take coluna (tabuleiro !! linha) ++ [jogador] ++ drop (coluna +  1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro


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

-- Main para permitir a execução do programa
main :: IO ()
main = do
    let tabuleiroPassado = tabuleiro4x4
    let tabuleiroPresente = tabuleiro4x4
    let tabuleiroFuturo = tabuleiro4x4

    jogar tabuleiroPassado tabuleiroPresente tabuleiroFuturo jogador1


