# Plutus Developer Environments



We have 4 options for developer environments:
* Demeter.run Plutus Stack
* Local Developer Environment based on Plutus Application Framework
* Docker Developer Environment based on IOH Plutus Pionneers Program Cohort 4 resources.
* Jambhala simplified local developer environment.

## Demeter.run

1. Open a Demeter.run account.
2. Create a new project.
* Choose organization
* Select cluster region
* Select Price Plan (Free tier is enough)
* Fill project name and description
* CREATE
3. Create a new workload
* Select Workspace
* Activate repository URL
  * https://github.com/EmurgoFaculty/EmurgoAcademyPlutusExamples.git
* Select Toolchain -> Plutus Tx 
* Select extras 
  * Cardano Binaries
  * Cabal Cache
* Workspace Size -> Large 
  * Change it later to small after building some initial dependencies.
* Network -> Preview 
* Fill the name 
* CREATE
  
######_Wait for priviosioning..._

4. Open VSCode
5. Navigate to folder 1_JustValidators and open a terminal or viceversa.
6. Test
``` 
        cabal update
        cabal repl
```

You should see the very nice prompt of the REPL

_Prelude AlwaysSucceedandFail>_



## Local developer environment

This guide was last tested with Ubuntu 22.04.2 LTS, other versions and platforms might not be exactly the same.

We recommend gNome desktop so you can use google chrome web wallets extensions like Nami


1.Recommended machine hardware:
* cpu:
  * minimun: 2 cores
  * recommended: 4 cores
* ram:
  * minimun: 16 GB
  * recommended: 32GB
* storage:
  * minimun: 64GB
  * recommended: 120GB (with full testnet node installation)

Native boot will work faster for all the below steps. Everything stays the same whether it's a VM or a native boot Ubuntu

You need a tool to retrieve files form the internet, like curl or wGet (this guide use curl), git and a text editor (this guide use nano for console, popular option is VScode for gui desktop).

1. Execute:
```    
        sudo apt-get update
        sudo apt-get upgrade
        sudo apt-get install -y snap git nano curl
```
 


#### Install Nix 

4. Execute
```    
        curl -L https://nixos.org/nix/install > install-nix.sh
        chmod +x install-nix.sh
        ./install-nix.sh

To ensure that the necessary environment variables are set, either log in again, or type

        . ~/.nix-profile/etc/profile.d/nix.sh
```
More advanced installations of nix can be found in https://nixos.org/download.html, but we haven't tested them so feel free to explore on your own.



5.	verify installations with --version
Execute
```
        nix --version
        git --version
        nano --version
```
#### IOG Binary Cache

6. IOHK Binary Cache (necesary for saving hours of time in the Plutus Libraries installation)

Execute:
```
    sudo mkdir -p /etc/nix
    sudo nano /etc/nix/nix.conf
```
Include on the open file this:
```    
experimental-features = nix-command flakes
substituters        = https://cache.iog.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=

```
(Reference: In nano editor, You can Save with Ctrl-O and close the file with Ctrl-X)

#### Setup PLUTUS-APPS

7.Clone the Plutus-Apps repository
Execute
```
    git clone https://github.com/input-output-hk/plutus-apps.git
    cd plutus-apps/
    git checkout tags/v1.1.0
```

8. Now, Launch the nix-shell
```
        nix-shell
        cabal update
```

9. Clone Emurgo Academy Plutus Examples repository
``` 
        git clone https://github.com/Vortecsmaster/EmurgoAcademyPlutusExamples.git
```
10. Quick test


<!--
## NOW INSTALL AND RUN THE PLUTUS PLAYGROUND
1. In the same window change the directory by executing

```
        cd plutus-playground-server
        cabal update
        plutus-playground-server
```

Takes a while and ends with the text:
```
    Interpreter ready 
```  
  
 10. Open a *new* terminal (on the GUI or a new SSH connection if no gui)

11. Change into the plutus-apps and open nix
Execute
```

    cd plutus-apps/
    nix-shell
```    
    
12. Change into plutus-playground-client folder, update and start
```
        cd plutus-playground-client
        cabal update
        npm start
```
You might use a browser to navigate to https://localhost:8009 and be able to see the Plutus Application Playground, try compiling and running the test contract to see if you were successfull. -->


#### Building the Plutus Documentation 


Navigate to the plutus-apps folder and open another nix-shell 
Execute
 ```   
    build-and-serve-docs
```
This will build the plutus documentation. 
Once it is running, open up your browser and navigate to http://0.0.0.0:8002/haddock. If this site doesn't work, navigate to http://localhost:8002/haddock

