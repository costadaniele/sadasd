# Tópicos Especiais em Sistemas para Internet III

- Alunos
   - Daniele Costa, Jonathas Ribeiro, Rosangela da Silva


## Para sistema Windows

- Abrir terminal PowerShell como Administrador

`wsl --install`

- Instalar nix

`sh <(curl -L https://nixos.org/nix/install) --daemon`

- Instalar ghc

`nix-env -iA nixpkgs.ghc`

Comando `ghci` para entrar no Haskell

- Obelisk

`ob run -v`

Referências:

https://docs.microsoft.com/pt-br/windows/wsl/install

https://hoogle.haskell.org/
