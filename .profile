# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# SSH
eval `keychain --eval --agents ssh ~/.ssh/cosmc_rsa`

# GVM (go managed installations)
[[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"

# Emacs
export TERM=xterm-256color
alias emacs='emacs -nw'

# Editor configuration
alias vi='nvim'
alias vim='nvim'
export EDITOR=vim
export VIEWER=vim
export PAGER=cat

# Configure the local scripts in the path
export PATH=$PATH:$HOME/bin

# Docker
alias docker='docker --tls'
alias docker_cleanup_exited='docker rm -v $(docker ps -a -q -f status=exited)'
alias minikube_docker='eval $(minikube docker-env)'
# export DOCKER_CERT_PATH=$HOME/.docker/
# export DOCKER_SSL_VERIFY=false

# Git branch in comman line
function parse_git_branch () {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

RED="\[\033[0;31m\]"
YELLOW="\[\033[0;33m\]"
GREEN="\[\033[0;32m\]"
NO_COLOR="\[\033[0m\]"

PS1="$GREEN\u@\h$NO_COLOR:\W$YELLOW\$(parse_git_branch)$NO_COLOR> "

# Go
[[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH="$PATH":$GOROOT/bin

# Maven
alias mvnFast='mvn -DskipTests=true -DskipJavadoc=true -Dgwt.compiler.localWorkers=3 -P dev -T 2'
alias mvnFastest='mvn -DskipDebianPackaging=true -Dsindbad.profile=dev -DskipJavadoc=true -DskipTests=true -Dgwt.compiler.localWorkers=3 -P dev -T 2'
alias mvnFastTest='mvn -DskipJavadoc=true -Dgwt.compiler.localWorkers=3 -P dev -T 2'

mvn_change_version() {
  mvn versions:set -DgenerateBackupPoms=false -DnewVersion=$1
}

alias mvnVersion=mvn_change_version

# Jenv
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"
export JAVA_HOME="$(jenv javahome)"

# JMeter
export PATH="$PATH:$HOME/tools/jmeter/current/bin"

# Cargo
export PATH="$PATH:$HOME/.cargo/bin"

# RVM
#export PATH="$HOME/.rvm/bin:$PATH" # Add RVM to PATH for scripting

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

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

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
