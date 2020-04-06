export CLICOLOR=cons25

# YoubiKey configuration for ssh key
# export SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"
# gpg-connect-agent updatestartuptty /bye
#
# SSH agent
eval `keychain --eval --agents ssh ~/.ssh/id_rsa`

# Binaries paths
export PATH=/usr/local/bin:/opt/local/bin:/opt/local/sbin:${HOME}/bin:$PATH

# Editor
alias vi='nvim'
alias vim='nvim'
export EDITOR=nvim
export VIEW=nvim
export PYTHON3_HOST_PROG='/usr/local/bin/python3'

# Watch
alias w='watch -c -t'

# C++
alias t='ctags -R --c++-kinds=+p --fields=+iaS --extra=+q `find . -name "*.c" -o -name "*.cc" -o -name "*.cpp" -o name "*.h" -o -name "*.hpp"`'
export PATH="/usr/local/opt/llvm/bin:$PATH"

# Go
[[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH="$PATH":$GOROOT/bin
export GO111MODULE="on"

# Python
alias pydb='python -m pudb.run'
export PATH=$PATH:$HOME/Library/Python/3.7/bin

# Rust
export PATH="$PATH:$HOME/.cargo/bin"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

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

# GitHub
alias hpr='hub pull-request'

# Anchore
alias anc='anchore-cli'
