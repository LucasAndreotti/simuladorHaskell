import Data.List
import Data.Maybe

-- Ler a memória
-- Retornar o conteúdo do endereço de memória
-- readMem(memoria,endereco)=conteudo
readMem :: [(Int,Int)] -> Int -> Int
readMem (x:xs) adress
    | adress == fst x = snd x
    | adress /= fst x = readMem xs adress

-- Escreve na memória
-- Retornar o conteúdo do endereço de memória
-- readMem(memoria,endereco,conteudo)=memoria
writeMem :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
writeMem (x:xs) adress content
    | adress == fst x = [(adress,content)] ++ xs 
    | adress /= fst x = [x] ++ (writeMem xs adress content)

-- Pegar o índice
getIndex :: [(Int,Int)] -> (Int,Int) -> Int
getIndex prog tuple = fromMaybe (-1) $ elemIndex tuple prog

-- LOD
-- lod(memoria,endereco)=memoria
lod :: [(Int,Int)] -> Int -> [(Int,Int)]
lod prog adress = writeMem prog 999 (readMem prog (readMem prog adress))

-- STO
-- sto(memoria,endereco)=memoria
sto :: [(Int,Int)] -> Int -> [(Int,Int)]
sto prog adress = writeMem prog (readMem prog adress) (readMem prog 999)

-- CPL
-- cpl(memoria,endereco)=memoria
cpl :: [(Int,Int)] -> Int -> [(Int,Int)]
cpl prog adress = if (readMem prog (readMem prog adress)) < 0 then writeMem prog 999 1
                  else writeMem prog 999 0

-- AND
-- and(memoria,endereco)=memoria
and' :: [(Int,Int)] -> Int -> [(Int,Int)]
and' prog adress = if (readMem prog (readMem prog adress)) == 0 && (readMem prog 999) == 0 then writeMem prog 999 1
                  else writeMem prog 999 0

-- ADD
-- add(memoria,endereco)=memoria
add :: [(Int,Int)] -> Int -> [(Int,Int)]
add prog adress = if ((readMem prog (readMem prog adress)) + (readMem prog 999)) <= 255 then writeMem prog 999 ((readMem prog (readMem prog adress)) + (readMem prog 999))
                  else writeMem prog 999 0

-- SUB
-- sub(memoria,endereco)=memoria
sub :: [(Int,Int)] -> Int -> [(Int,Int)]
sub prog adress = if ((readMem prog 999) - (readMem prog (readMem prog adress))) > 0 then writeMem prog 999 ((readMem prog 999) - (readMem prog (readMem prog adress)))
                  else writeMem prog 999 0
