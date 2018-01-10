# Bomberman Clone

Este repositório contém o código e documentação do projeto da cadeira Laboratórios de Informática I (2016/2017).

O projeto é composto por 6 tarefas:
- Tarefa 1: construir o mapa em representação de texto
- Tarefa 2: fazer o jogador mover um turno dado um comando
- Tarefa 3: codificar e descodificar o mapa em representação de texto para efeitos de compressão
- Tarefa 4: fazer o tempo avançar um turno
- Tarefa 5: implementar o jogo usando o Gloss
- Tarefa 6: programar um bot para jogar o jogo

# Instalar as dependências (Linux)

gloss:
```
cabal install gloss
```
gloss-juicy:
```
cabal install gloss-juicy
```
freeglut3:
```
sudo apt install freeglut3
```

# Executar o jogo

```
cd src/
ghc Tarefa5.hs
./Tarefa5.hs
```
