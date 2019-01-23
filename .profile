export CLICOLOR=cons25

# YoubiKey configuration for ssh key
export SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"
gpg-connect-agent updatestartuptty /bye

# SSH agent
#eval `keychain --eval --agents ssh ~/.ssh/cosmc_rsa`

# Binaries paths
export PATH=/opt/local/bin:/opt/local/sbin:${HOME}/bin:$PATH
export PATH=$PATH:~/bin:/usr/local/bin

# Brew
export PATH="/usr/local/bin:$PATH"

# Emacs
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
export TERM=xterm-256color

# Editor
alias vi='nvim'
alias vim='nvim'
export EDITOR=nvim
export VIEW=nvim

# Watch
alias w='watch -c -t'

# Ctags
alias t='ctags -R --c++-kinds=+p --fields=+iaS --extra=+q `find . -name "*.c" -o -name "*.cc" -o -name "*.cpp" -o name "*.h" -o -name "*.hpp"`'

# Docker
#alias docker='docker --tls'
alias minikube_docker='eval $(minikube docker-env)'
alias docker_cleanup_exited='docker rm -v $(docker ps -a -q -f status=exited)'
alias docker_remove_all_containers='docker rm -f $(docker ps -a -q)'
alias docker_remove_all_images='docker rmi -f $(docker images -q)'

# Go
[[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"
export PATH=$PATH:/usr/local/opt/go/libexec/bin
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH="$PATH":$GOROOT/bin

# Python
export PATH=$PATH:$HOME/Library/Python/2.7/bin
alias pydb='python -m pudb.run'

# Jenv
export PATH=$PATH:~/.jenv/bin
eval "$(jenv init -)"

# Java
export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_112.jdk/Contents/Home"

# Maven
alias mvnFast='mvn -DskipTests=true -DskipJavadoc=true -Dgwt.compiler.localWorkers=3 -P dev -T 2'
alias mvnFastest='mvn -DskipDebianPackaging=true -Dsindbad.profile=dev -DskipJavadoc=true -DskipTests=true -Dgwt.compiler.localWorkers=3 -P dev -T 2'
alias mvnFastTest='mvn -DskipJavadoc=true -Dgwt.compiler.localWorkers=3 -P dev -T 2'

mvn_change_version() {
    mvn versions:set -DgenerateBackupPoms=false -DnewVersion=$1
}

alias mvnVersion=mvn_change_version

# Rust
export PATH="$PATH:$HOME/.cargo/bin"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

# Ruby
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# Eyaml
alias eyaml_dse1='ln -sf ~/.eyaml/dse1/config.yaml ~/.eyaml/config.yaml'
alias eyaml_dse2='ln -sf ~/.eyaml/dse2/config.yaml ~/.eyaml/config.yaml'
alias eyaml_edit='eyaml edit'

# Helper functions for Azure KeyVault
find_secret() {
	az keyvault secret list --vault-name $1 | jq -r --arg PATTERN "$2" '.[] | select(.contentType | . and test($PATTERN; "i")) | "Id: \(.id) \n\(.contentType)"'
}

get_secret() {
	az keyvault secret show --vault-name $1 --name $2 | jq .value | sed -e 's/^"//' -e 's/"$//'
}

set_secret() {
	az keyvault secret set --vault-name $1 --name $2 --value "${3}"
}

alias find-secret=find_secret
alias get-secret=get_secret
alias set-secret=set_secret

# Kubernetes
alias k=kubectl
alias h=helm
# Get and decode a kuberntes secret
ksecret() {
    kubectl get secert -o yaml $1 | ksd
}

# List all crds for a specific pattern
crds() {
    kubectl get crds | grep $1 | awk '{print $1}' 
}

# List all crds
acrds() {
    kubectl get crds
}

# Delete all crds for a specific pattern
dcrds() {
    kubectl get crds | grep $1 | awk '{print $1}' | while read crd; do kubectl delete crd $crd; done
}

# Delete and purge a helm chart release
hdp() {
    helm delete --purge $1
}
