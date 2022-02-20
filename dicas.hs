-- Define a memoria como uma colecao de tuplas (endereco,valor)
-- [(Int,Int)]

-- Exemplo:
-- 0 LOD 240
-- 2 ADD 241
-- 4 STO 251
-- 6 HLT NOP
-- Observação: a posição 999 é o ACC
-- let prog1 = [(0,2),(1,240),(2,14),(3,241),(4,4),(5,251),(6,20),(7,18),(240,0),(241,1),(251,0),(999,0)]
-- simular prog1
-- simular :: [(Int,Int)] -> [(Int,Int)]

-- Ler a memória
-- Retornar o conteúdo do endereço de memória
-- readMem(memoria,endereco)=conteudo
readMem :: [(Int,Int)] -> Int -> Int
readMem (x:xs) e
    | e == fst x = snd x
    | e /= fst x = readMem xs e

-- Escrever na memória
-- Armazenar o conteúdo em um endereço de memória
-- writeMem(memoria,endereço,conteudo)=memoria