## Worksapces Structure

- O que é um workspace?
- Projetos dento de um projeto global deve ser considerado um workspace?


### Algumas Estrutura Monorepo

1. Like Yarn Workspaces

.
|-- package.json
|-- packages
|   |-- dep01
|   |-- dep02
|   |-- main
|-- bsonconfig.json

2.

.
|-- app
|-- server



- Cada workspace (inclui a raiz global) tem um FSEvent para `lib/bs/.compiler.log`
- A Compilação é feita na raiz global `node_modules/rescript/<platformt>/rescript.exe build -with-deps` quando o documento é salvo

### Objetivos

- Analisar o `bsconfig.json` e extrar `pinne-dependencies` e resolver os caminhos. 
Aborgagem mais complicada, mas correta. Na forma atual preciso encontrar o 
`bsconfig.json` toda vez que o arquivo é aberto. 

Isso gera o seguinte problema: se uma pinned-deps tem um bug e abro um arquivo 
de outra pinned-deps e faço o build with-deps ele não é detectado porque não abri
o arquivo e o LSP não esta assintindo log. Os workspace tem que ser resolvidos ao
abrir o primeito arquivo.

1. Ler bsconfig.json e extrair pinned-deps
2. Resolver `workspaces` no package.json

- Devo adicionar ao workspace deps dentro de `node_modules`? `rust_analyzer`
adiciona.


```bash
npm ls --workspaces --json
```

## Single Code Base

- Não tem workspaces, apenas um global root

### Questions

- Nessa estrutura devo compilar com o argumento `-with-deps`?


## Build

- Create a child_process
- Run process when save text document

## Questions

- Quanto um arquivo é fechado devo excluir o FileWatcher para aquele workspace?
	- Problemas:
		- Se um arquivo é renomeado e afeta dependencias não é possível captar erros reportado no log

- A execução do comando nativo afeta a execução do comando pelo node?
- Como `rust_analyzer` lida com workspaces?
