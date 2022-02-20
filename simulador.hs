-- PROGRAMAÇÃO FUNCIONAL
-- PROFESSOR JÚLIO HENRIQUE
-- GABRIELA ZORZO E LUCAS ANDREOTTI
-- TURMA 010

import Data.List
import Data.Maybe

-- OBSERVAÇÕES:
-- A posição 999 é o ACC
-- A posição 1000 armazena o endereço da próxima instrução a ser executada
-- Executar com o ghci, colocando a declaração de cada prog no terminal

-- EXEMPLO DE EXECUÇÃO:
-- ghci
-- :load Simulador.hs
-- let prog1 = [(0,2),(1,240),(2,14),(3,241),(4,4),(5,251),(6,20),(7,18),(240,5),(241,6),(251,0),(999,0),(1000,0)]
-- simulator prog1

-- TESTES:
-- Resp = A + B
-- assembler: 0 LOD 240 | 2 ADD 241 | 4 STO 251 | 6 HLT NOP
-- let prog1 = [(0,2),(1,240),(2,14),(3,241),(4,4),(5,251),(6,20),(7,18),(240,5),(241,6),(251,0),(999,0),(1000,0)
-- resultado esperado: [(0,2),(1,240),(2,14),(3,241),(4,4),(5,251),(6,20),(7,18),(240,5),(241,6),(251,11),(999,11),(1000,6)

-- Resp = A + B - 2
-- assembler: 0 LOD 240 | 2 ADD 241 | 4 SUB 245 | 6 STO 251 | 8 HLT NOP
-- let prog2 = [(0,2),(1,240),(2,14),(3,241),(4,16),(5,245),(6,4),(7,251),(8,20),(9,18),(240,5),(241,6),(245,2),(251,0),(999,0),(1000,0)]
-- resultado esperado: [(0,2),(1,240),(2,14),(3,241),(4,16),(5,245),(6,4),(7,251),(8,20),(9,18),(240,5),(241,6),(245,2),(251,9),(999,9),(1000,8)]

-- Resp = A * B
-- assembler: 0 LOD 241 | 2 SUB 245 | 4 STO 241 | 6 CPL 999 | 8 JMZ 12 | 10 HLT NOP | 12 LOD 251 | 14 ADD 240 | 16 STO 251 | 18 JMP 0
-- let prog7 = [(0,2),(1,241),(2,16),(3,245),(4,4),(5,241),(6,10),(7,999),(8,8),(9,12),(10,20),(11,18),(12,2),(13,251),(14,14),(15,240),(16,4),(17,251),(18,6),(19,0),(240,3),(241,2),(245,1),(251,0),(999,0),(1000,0)]
-- resultado esperado: [(0,2),(1,241),(2,16),(3,245),(4,4),(5,241),(6,10),(7,999),(8,8),(9,12),(10,20),(11,18),(12,2),(13,251),(14,14),(15,240),(16,4),(17,251),(18,6),(19,0),(240,3),(241,-1),(245,1),(251,6),(999,1),(1000,10)]

-- if (A > B) Resp = A - B; else Resp = B - A
-- assembler: O LOD 240 | 2 SUB 241 | 4 CPL 999 | 6 JMZ 16 | 8 LOD 241 | 10 SUB 240 | 12 STO 251 | 14 HLT NOP | 16 LOD 240 | 18 SUB 241 | 20 STO 251 | 22 HLT NOP
-- Teste 1: A > B
-- let prog4 = [(0,2),(1,240),(2,16),(3,241),(4,10),(5,999),(6,8),(7,16),(8,2),(9,241),(10,16),(11,240),(12,4),(13,251),(14,20),(15,18),(16,2),(17,240),(18,16),(19,241),(20,4),(21,251),(22,20),(23,18),(240,20),(241,5),(251,0),(999,0),(1000,0)]
-- resultado esperado: [(0,2),(1,240),(2,16),(3,241),(4,10),(5,999),(6,8),(7,16),(8,2),(9,241),(10,16),(11,240),(12,4),(13,251),(14,20),(15,18),(16,2),(17,240),(18,16),(19,241),(20,4),(21,251),(22,20),(23,18),(240,20),(241,5),(251,15),(999,15),(1000,22)]
-- Teste 2: B > A
-- let prog5 = [(0,2),(1,240),(2,16),(3,241),(4,10),(5,999),(6,8),(7,16),(8,2),(9,241),(10,16),(11,240),(12,4),(13,251),(14,20),(15,18),(16,2),(17,240),(18,16),(19,241),(20,4),(21,251),(22,20),(23,18),(240,3),(241,5),(251,0),(999,0),(1000,0)]
-- resultado esperado: [(0,2),(1,240),(2,16),(3,241),(4,10),(5,999),(6,8),(7,16),(8,2),(9,241),(10,16),(11,240),(12,4),(13,251),(14,20),(15,18),(16,2),(17,240),(18,16),(19,241),(20,4),(21,251),(22,20),(23,18),(240,3),(241,5),(251,2),(999,2),(1000,14)]

-- A = 0; Resp = 1; while (A < 10) { A = A + 1; Resp = Resp + 2}    
-- assembler: 0 LOD 245 | 2 SUB 240 | 4 CPL 999 | 6 JMZ 10 | 8 HLT NOP | 10 LOD 240 | 12 ADD 246 | 14 STO 240 | 16 LOD 251 | 18 ADD 247 | 20 STO 251 | 22 JMP 0
-- let prog6 = [(0,2),(1,245),(2,16),(3,240),(4,10),(5,999),(6,8),(7,10),(8,20),(9,18),(10,2),(11,240),(12,14),(13,246),(14,4),(15,240),(16,2),(17,251),(18,14),(19,247),(20,4),(21,251),(22,6),(23,0),(240,0),(245,9),(246,1),(247,2),(251,1),(999,0),(1000,0)] 
-- resultado esperado: [(0,2),(1,245),(2,16),(3,240),(4,10),(5,999),(6,8),(7,10),(8,20),(9,18),(10,2),(11,240),(12,14),(13,246),(14,4),(15,240),(16,2),(17,251),(18,14),(19,247),(20,4),(21,251),(22,6),(23,0),(240,10),(245,9),(246,1),(247,2),(251,21),(999,1),(1000,8)]

-- Simulador
-- simulator prog1
simulator :: [(Int,Int)] -> [(Int,Int)]
simulator prog = if (readMem prog (readMem prog 1000)) == 20 then hlt prog
                 else simulator (instruction prog (readMem prog 1000))

-- Instrucao
instruction :: [(Int,Int)] -> Int -> [(Int,Int)]
instruction prog adress = if (readMem prog adress) == 2 then lod prog (adress + 1) -- LOD
                     else if (readMem prog adress) == 4 then sto prog (adress + 1) -- STO
                     else if (readMem prog adress) == 6 then jmp prog (adress + 1) -- JMP
                     else if (readMem prog adress) == 8 then jmz prog (adress + 1) -- JMZ
                     else if (readMem prog adress) == 10 then cpl prog (adress + 1) -- CPL
                     else if (readMem prog adress) == 12 then and' prog (adress + 1) -- AND
                     else if (readMem prog adress) == 14 then add prog (adress + 1) -- ADD
                     else if (readMem prog adress) == 16 then sub prog (adress + 1) -- SUB
                     else if (readMem prog adress) == 18 then nop prog -- NOP
                     else if (readMem prog adress) == 20 then hlt prog -- HLT
                     else prog                   

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

-- LOD
-- lod(memoria,endereco)=memoria
lod :: [(Int,Int)] -> Int -> [(Int,Int)]
lod prog adress = writeMem (writeMem prog 999 (readMem prog (readMem prog adress))) 1000 (adress + 1)

-- STO
-- sto(memoria,endereco)=memoria
sto :: [(Int,Int)] -> Int -> [(Int,Int)]
sto prog adress = writeMem (writeMem prog (readMem prog adress) (readMem prog 999)) 1000 (adress + 1)

-- JMP
-- jmp(memoria,endereco)=memoria
jmp :: [(Int,Int)] -> Int -> [(Int,Int)]
jmp prog adress = writeMem prog 1000 (readMem prog adress)

-- JMZ
-- jmz(memoria,endereco)=memoria
jmz :: [(Int,Int)] -> Int -> [(Int,Int)]
jmz prog adress = if (readMem prog 999) == 0 then writeMem prog 1000 (readMem prog adress)
                  else writeMem prog 1000 (adress + 1)

-- CPL
-- cpl(memoria,endereco)=memoria
cpl :: [(Int,Int)] -> Int -> [(Int,Int)]
cpl prog adress = if (readMem prog (readMem prog adress)) < 0 then writeMem (writeMem prog 999 1) 1000 (adress + 1)
                  else writeMem (writeMem prog 999 0) 1000 (adress + 1)

-- AND
-- and(memoria,endereco)=memoria
and' :: [(Int,Int)] -> Int -> [(Int,Int)]
and' prog adress = if (readMem prog (readMem prog adress)) == 0 && (readMem prog 999) == 0 then writeMem (writeMem prog 999 1) 1000 (adress + 1)
                  else writeMem (writeMem prog 999 0) 1000 (adress + 1)

-- ADD
-- add(memoria,endereco)=memoria
add :: [(Int,Int)] -> Int -> [(Int,Int)]
add prog adress = if ((readMem prog (readMem prog adress)) + (readMem prog 999)) <= 127 then writeMem (writeMem prog 999 ((readMem prog (readMem prog adress)) + (readMem prog 999))) 1000 (adress + 1)
                  else writeMem (writeMem prog 999 0) 1000 (adress + 1)

-- SUB
-- sub(memoria,endereco)=memoria
sub :: [(Int,Int)] -> Int -> [(Int,Int)]
sub prog adress = if ((readMem prog 999) - (readMem prog (readMem prog adress))) > (-128) then writeMem (writeMem prog 999 ((readMem prog 999) - (readMem prog (readMem prog adress)))) 1000 (adress + 1)
                  else writeMem (writeMem prog 999 0) 1000 (adress + 1)

-- NOP
-- usado para completar a condição de parada
nop :: [(Int,Int)] -> [(Int,Int)]
nop prog = prog

-- HLT
-- condição de parada
hlt :: [(Int,Int)] -> [(Int,Int)]
hlt prog = prog
